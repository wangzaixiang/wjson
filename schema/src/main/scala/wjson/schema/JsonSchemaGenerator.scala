package wjson.schema

import wjson.JsValue.JsObject

import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.quoted.*
import wjson.*

import scala.annotation.tailrec

object JsonSchemaGenerator:

  class MyInspector(quotes: Quotes)(tastys: List[Tasty[quotes.type]]):
    import quotes.reflect.*
    given Quotes = quotes

    private object ADTType:
      def applyEnum(symbol: Symbol): SumType = {
        val body = symbol.tree.asInstanceOf[ClassDef].body
        body.find { // find fitsy Import
          case Import(_, _) => true
          case _ => false
        } match
          case Some(x@Import(term, selectors)) => // import Color.{Red, Green, Blue, RGB}
            val items: List[ADTType.CaseProduct | ADTType.CaseSimple] = selectors.map {
              case SimpleSelector(name) =>
                val isSingle = term.symbol.declaredType(name) == Nil // Red, Green, Blue
                if isSingle then
                  ADTType.CaseSimple(term.symbol.declaredField(name))
                else  // case Mixed(rgb)
                  val tpe = term.symbol.declaredType(name)(0) // term:Color tpe: symbol: Color.RGB
                  ADTType.CaseProduct(tpe, tpe.caseFields)
                end if
              case _ => ??? // Only simple selectors are supported
            }
            ADTType.SumType(symbol, items)
          case _ => ??? // No import
      }

      def applyCaseClass(symbol: Symbol): CaseProduct =
        val fields = symbol.caseFields
        ADTType.CaseProduct(symbol, fields)

    private enum ADTType:
      case SumType(symbol: Symbol, items: List[ADTType.CaseProduct | ADTType.CaseSimple])
      case CaseProduct(symbol: Symbol, fields: List[Symbol])
      case CaseSimple(symbol: Symbol)

    private val tree: Tree = tastys(0).ast

    private def extractTypes(tree: quotes.reflect.Tree): List[ADTType] = {
      tree match
        case PackageClause(pid, stats) =>
          stats.flatMap {
            case classDef@ClassDef(name, constr, parents, selfOpt, body) => {
              val symbol = classDef.symbol
              val isCase = symbol.flags.is(Flags.Case)
              val isEnum = symbol.flags.is(Flags.Enum)
              val isSynthetic = symbol.flags.is(Flags.Synthetic)

              if isEnum && !isSynthetic then // SUM TYPE
                val sum = ADTType.applyEnum(symbol)
                Some(sum)

              else if isCase && !isSynthetic then // PRODUCT TYPE
                Some( ADTType.applyCaseClass(symbol) )
              else
                None
              end if
            }
            case _ => Nil
          }
        case _ => Nil
    }

    private def schemaOfType(tpe: TypeRepr, ref: Boolean, definitions: collection.mutable.Set[ADTType]): JsObject = tpe match
        case x if x <:< TypeRepr.of[String] =>
          JsObject("type" -> JsString("string"))
        case x if x =:= TypeRepr.of[Int] || x =:= TypeRepr.of[Short] || x =:= TypeRepr.of[Long] =>
          JsObject("type" -> JsString("integer"))
        case x if x =:= TypeRepr.of[Float] || x =:= TypeRepr.of[Double] =>
          JsObject("type" -> JsString("integer"))
        case x if x =:= TypeRepr.of[Boolean] =>
          JsObject("type" -> JsString("boolean"))
        case x if x.typeSymbol.flags.is(Flags.Enum) =>
          if ref then
            definitions.add(ADTType.applyEnum(x.typeSymbol))
            JsObject("$ref" -> JsString("#/definitions/" + x.typeSymbol.fullName))
          else
            val sum = ADTType.applyEnum(x.typeSymbol)
            schemaOfSumType(sum, definitions)
        case x if x.typeSymbol.flags.is(Flags.Case) && !x.typeSymbol.flags.is(Flags.Synthetic) =>
          // TODO check List/Array etc
          if ref then
            definitions.add(ADTType.applyCaseClass(x.typeSymbol))
            JsObject("$ref" -> JsString("#/definitions/" + x.typeSymbol.fullName))
          else
             val product = ADTType.applyCaseClass(x.typeSymbol)
              schemaOfProductCase(product, includeKind = false, definitions)
          end if
        case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.collection.immutable.List").typeRef =>
          JsObject("type" -> JsString("array"),
            "items" -> schemaOfType(args(0), ref = true, definitions))
        case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.collection.immutable.Map").typeRef =>
          JsObject("type" -> JsString("object"),
            "additionalProperties" -> schemaOfType(args(1), ref = true, definitions) )
        case _ =>
          val sym = tpe.typeSymbol
          if sym.flags.is(Flags.Opaque) && sym.isType  then
            sym.tree match
              case TypeDef(_, rhs) =>
                schemaOfType(rhs.symbol.typeRef, true, definitions)
          else
            assert(false, s"Unsupported type: ${tpe.show} - ${tpe.show(using Printer.TypeReprStructure)}")
            ???

    /**
     * { type: "object",  properties: { }, additionalProperties: false, required: [ ] }
     *
     */
    private def schemaOfProductCase(product: ADTType.CaseProduct, includeKind: Boolean, definitions: collection.mutable.Set[ADTType]): JsObject =
      val params =
        val primaryConstructor = product.symbol.primaryConstructor
        primaryConstructor.tree.asInstanceOf[DefDef].paramss(0).params.asInstanceOf[List[ValDef]]

      val fields: List[(String, JsValue)] = product.fields.zip(params).map { (field, param) =>
        // TODO primitive
        // TODO array/map support
        // TODO recursive types

        val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe

        val typeObj = schemaOfType(tpe, ref = true, definitions)

        val name = field.name

        val description: Option[String] =
          param.symbol.annotations
            .find(_.tpe.typeSymbol == Symbol.requiredClass("wjson.schema.JsonSchema.description"))
            .map { case Apply(_, List(Literal(StringConstant(str)))) => str }
        val desc = description match
            case Some(description) => JsObject("description" -> JsString(description))
            case None => JsObject()

        name -> (typeObj ++ desc)
      }
      val kind: List[(String, JsValue)] =
        if includeKind then
          List("_kind" -> JsObject("type" -> JsString("string"),
            "enum" -> JsArray(JsString(product.symbol.name))))
        else Nil

      val required = product.fields.map(f => f.name) ++ (if (includeKind) List("_kind") else Nil)
      JsObject(
        "type" -> JsString("object"),
        "properties" -> new JsObject(kind ++ fields),
        "additionalProperties" -> JsBoolean(false),
        // "required" -> JsArray(required.map(JsString(_)): _*)  // TODO
      )

    private def schemaOfSumType(sum: ADTType.SumType, definitions: collection.mutable.Set[ADTType]): JsObject =
      JsObject( "oneOf" -> JsArray(
          sum.items.map {
            case ADTType.CaseSimple(symbol) =>
              JsObject( "type" -> JsString("string"), "enum" -> JsArray(JsString(symbol.name)) )
            case x @ ADTType.CaseProduct(symbol, fields) =>
              schemaOfProductCase(x, includeKind = true, definitions)
          }
        ) )

    private val result = extractTypes(tree)

    private val definitions = collection.mutable.Set[ADTType]()

    val root: JsObject = result(0) match
      case sum@ADTType.SumType(typeSymbol, items) =>
        val schema = schemaOfSumType(sum, definitions)
        JsObject(
            "$schema" -> JsString("http://json-schema.org/draft-07/schema#"),
            "$id" -> JsString(typeSymbol.fullName)
        ) ++ schema

      case prd@ADTType.CaseProduct(symbol, fields) =>
        val schema = schemaOfProductCase(prd, includeKind = false, definitions)
        JsObject(
            "$schema" -> JsString("http://json-schema.org/draft-07/schema#"),
            "$id" -> JsString(symbol.fullName)
        ) ++ schema

      case x: ADTType.CaseSimple =>
        throw new UnsupportedOperationException(s"Unsupported type: ${x}")

    @tailrec
    private def rescure(root: JsObject, remains: collection.mutable.Set[ADTType], processed: Set[ADTType]): JsObject =
      if remains.isEmpty then root
      else
        val (one, others) = (remains.head, remains.tail)
        if( processed.contains(one) ) rescure(root, others, processed)
        else
          val definition: (String, JsObject) = one match
            case x@ADTType.CaseProduct(symbol, fields) =>
              symbol.fullName -> schemaOfProductCase(x, includeKind = false, others)
            case x@ADTType.SumType(symbol, items) =>
              symbol.fullName -> schemaOfSumType(x, others)
            case x: ADTType.CaseSimple =>
              throw new UnsupportedOperationException(s"Unsupported type: ${x}")
          val root2: JsObject =
             if( root.contains("definitions") )
               val oldDefinition = root.field("definitions").asInstanceOf[JsObject]
               root + ("definitions" -> (oldDefinition + definition) )

             else JsObject(root.fields ++ List("definitions" -> JsObject(definition)))

          rescure(root2, others, processed+one)

    val root2 = rescure(root, definitions, Set.empty)

    println(root2.show())

    //
  end MyInspector

  class JsonSchemaInspector extends Inspector:

    override def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      new MyInspector(quotes)(tastys)
      ()

  def main(args: Array[String]): Unit =
    val inspector = new JsonSchemaInspector
//    val tastyFiles = List("schema/target/scala-3.2.2/test-classes/wjson/schema/test/Color.tasty")
    //    val tastyFiles = List("schema/target/scala-3.2.2/test-classes/wjson/schema/test/Person.tasty")
    val tastyFiles = List("/Users/wangzaixiang/workspaces/wangzaixiang/cube_design/target/scala-3.2.2/classes/tabular/model/TabularModel.tasty")
    TastyInspector.inspectTastyFiles(tastyFiles)(new JsonSchemaInspector)

