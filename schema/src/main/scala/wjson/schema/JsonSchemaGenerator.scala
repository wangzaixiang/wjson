package wjson.schema

import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.quoted.*
import wjson.*

import java.io.{FileOutputStream, PrintWriter}
import scala.annotation.tailrec

object JsonSchemaGenerator:

  class TastySchemaLoader(quotes: Quotes)(tastys: List[Tasty[quotes.type]]):
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

    private def extractTopLevelADTTypes(tree: quotes.reflect.Tree): List[ADTType] = {
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

    // TODO optimize $ref
    private def schemaOfType(tpe: TypeRepr, ref: Boolean, definitions: collection.mutable.Set[ADTType]): JsObject = tpe match
        case x if x =:= TypeRepr.of[JsValue] =>
          JsObject("type" -> Array("null", "boolean", "integer", "number", "array", "object").toJson )
        case x if x =:= TypeRepr.of[JsValue.JsString] =>
          JsObject("type" -> JsString("string"))
        case x if x =:= TypeRepr.of[JsValue.JsNumber] =>
          JsObject("type" -> JsString("number"))
        case x if x =:= TypeRepr.of[JsValue.JsBoolean] =>
          JsObject("type" -> JsString("boolean"))
        case x if x =:= TypeRepr.of[JsValue.JsArray] =>
          JsObject("type" -> JsString("array"))
        case x if x =:= TypeRepr.of[JsValue.JsObject] =>
          JsObject("type" -> JsString("object"))

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
        case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.Array").typeRef =>
          JsObject("type" -> JsString("array"),
            "items" -> schemaOfType(args(0), ref = true, definitions))
        case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.collection.immutable.Map").typeRef =>
          JsObject("type" -> JsString("object"),
            "additionalProperties" -> schemaOfType(args(1), ref = true, definitions) )
        case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.Option").typeRef =>
          schemaOfType(args(0), false, definitions)
        case x@OrType(left, right) =>
          val l = schemaOfType(left, false, definitions)
          val r = schemaOfType(right, false, definitions)

          val flatL =
            if l.contains("oneOf") then l.field("oneOf").asInstanceOf[JsArray].elements.toList
            else List(l)

          val flatR =
            if r.contains("oneOf") then r.field("oneOf").asInstanceOf[JsArray].elements.toList
            else List(r)

          JsObject("oneOf" -> JsArray(flatL ++ flatR))

        case _ =>
          val sym = tpe.typeSymbol
          if sym.isType  then
            sym.tree match
              case TypeDef(_, rhs: Term) =>
                schemaOfType(rhs.symbol.typeRef, true, definitions)
              case TypeDef(_, rhs: TypeTree) =>
                schemaOfType(rhs.tpe, true, definitions)
              case _ =>
                println(s"Unsupported type: ${tpe.show} - ${tpe.show(using Printer.TypeReprStructure)}")
                ???
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
          List("_enum" -> JsObject("type" -> JsString("string"),
            "enum" -> JsArray(JsString(product.symbol.name))))
        else Nil

      val required = product.fields.map(f => f.name) ++ (if (includeKind) List("_enum") else Nil)
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
              val description: Option[String] = extractDescription(symbol)
              JsObject( "type" -> JsString("string"), "enum" -> JsArray(JsString(symbol.name)) )
                ++ description.map(desc => JsObject("description" -> JsString(desc))).getOrElse(JsObject())
            case x @ ADTType.CaseProduct(symbol, fields) =>
              val description: Option[String] = extractDescription(symbol)
              schemaOfProductCase(x, includeKind = true, definitions)
                ++ description.map(desc => JsObject("description" -> JsString(desc))).getOrElse(JsObject())
          }
        ) )

    private def extractDescription(symbol: Symbol): Option[String] =
      symbol.annotations
        .find(_.tpe.typeSymbol == Symbol.requiredClass("wjson.schema.JsonSchema.description"))
        .map { case Apply(_, List(Literal(StringConstant(str)))) => str }

    private val result = extractTopLevelADTTypes(tree)

    private val definitions = collection.mutable.Set[ADTType]()

    private val root: JsObject = result(0) match
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
    private def rescure(schemaRoot: JsObject, remains: collection.mutable.Set[ADTType], processed: Set[ADTType]): JsObject =
      if remains.isEmpty then schemaRoot
      else
        val (head, tail) = (remains.head, remains.tail)
        if( processed.contains(head) ) rescure(schemaRoot, tail, processed)
        else
          val definition: (String, JsObject) = head match
            case x@ADTType.CaseProduct(symbol, fields) =>
              symbol.fullName -> schemaOfProductCase(x, includeKind = false, tail)
            case x@ADTType.SumType(symbol, items) =>
              symbol.fullName -> schemaOfSumType(x, tail)
            case x: ADTType.CaseSimple =>
              throw new UnsupportedOperationException(s"Unsupported type: ${x}")
          val root2: JsObject =
             if( schemaRoot.contains("definitions") )
               val oldDefinition = schemaRoot.field("definitions").asInstanceOf[JsObject]
               schemaRoot + ("definitions" -> (oldDefinition + definition) )

             else JsObject(schemaRoot.fields ++ List("definitions" -> JsObject(definition)))

          rescure(root2, tail, processed+head)

    val schema: JsValue.JsObject = rescure(root, definitions, Set.empty)


    //
  end TastySchemaLoader

  object JsonSchemaInspector:
    def apply(tastyFile: String): JsObject =
      val inspector = new JsonSchemaInspector
      TastyInspector.inspectTastyFiles(List(tastyFile))(inspector)
      inspector.schema

  class JsonSchemaInspector extends Inspector:

    var schema: JsObject = _
    override def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      val loader = new TastySchemaLoader(quotes)(tastys)
      schema = loader.schema
      ()

  @main
  def Tasty2Schema(input: String, output: String): Unit =
    val schema = JsonSchemaInspector.apply(input)
    val out = new PrintWriter(new FileOutputStream(output))
    out.println(schema.show())
    out.close()

