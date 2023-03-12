package wjson

import com.sun.xml.internal.bind.v2.model.core.TypeRef
import jdk.nashorn.internal.ir.Terminal

import scala.quoted.*
import scala.deriving.*

/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object JsValueMapperMacro:

  inline def genADT[T: deriving.Mirror.Of]: JsValueMapper[T] = ${ genADTImpl[T] }

  private def extractTypeRepres[T: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    Type.of[T] match
      case '[t *: ts] => quotes.reflect.TypeRepr.of[t] :: extractTypeRepres[ts]
      case '[EmptyTuple] => Nil

  private def extractNames[T: Type](using Quotes): List[String] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: ts] => TypeRepr.of[t] match
        case ConstantType(StringConstant(name)) =>
          name :: extractNames[ts]
        case _ => throw new AssertionError("Expected a String constant type")
      case '[EmptyTuple] => Nil

  private def genADTImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*

    Expr.summon[Mirror.Of[T]].get match
      case '{ $m: Mirror.ProductOf[T] } =>
        genProduct[T]
      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames } } =>
        genSum[T, elemTypes, elemNames]
      case _ => throw new AssertionError("Expected a Product or a Sum")


  private def genSum[T: Type, elemTypes: Type, elemNames: Type](using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    val names = extractNames[elemNames]
    val typeReprs = extractTypeRepres[elemTypes]

    val nameTypes: List[(String, TypeRepr)] = names.zip(typeReprs)

    def getMapper(tpe: TypeRepr): Expr[JsValueMapper[_]] =
      tpe.asType match
        case '[t] => Expr.summon[JsValueMapper[t]] match
          case Some(mapper) => mapper
          case _ => throw new Exception("No mapper found for type " + tpe.show(using Printer.TypeReprStructure))

    // "Mixed" -> Expr[JsValueMapper[Mixed]]
    val prodMappersByName: List[(String, Term)] = nameTypes.filter { case (_, tpe) => tpe.termSymbol == Symbol.noSymbol }
      .map { case (name, tpe) => (name, getMapper(tpe).asTerm) }
    val prodMappers: List[Term] = prodMappersByName.map(_._2)
    val prodMapperIndexByName: Map[String, Int] = prodMappersByName.zipWithIndex.map { case ((name, _), index) => (name, index) }.toMap

    /**
     * <pre>
     *    def fromJson(json: JsValue): Color = json match
     *      case JsString("Red") => Red
     *      ...
     *      case x:JsObject if x.fields("_kind") == "Mixed" => summon[Mixed].fromJson(json)
     * </pre>
     */
    def fromJsonImpl(refs: List[Ref], js: Expr[JsValue]): Expr[T] =
      val cases = nameTypes.map { arg =>
        val name: String = arg._1
        val tpe: TypeRepr = arg._2
        if (tpe.termSymbol != Symbol.noSymbol) { // case Red of single value
          val selector = Ref(Symbol.requiredModule("wjson.JsValue.JsString"))
          val tree = Unapply(Select.unique(selector, "unapply"), Nil, List(Literal(StringConstant(name))))
          val tpt = Inferred(TypeRepr.of[JsString])
          val pattern = TypedOrTest(tree, tpt)
          CaseDef(pattern, None, Ref(tpe.termSymbol)) // case JsString("Red") => Red
        }
        else { // case Mixed(rgb) of a case class
          val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[JsValue.JsObject], Flags.EmptyFlags, Symbol.noSymbol)
          val bindPattern = Typed(Wildcard(), TypeTree.of[JsValue.JsObject])
          val pattern = Bind(sym, bindPattern)
          val xExpr = Ref(sym).asExprOf[JsValue.JsObject]
          val kind: Expr[String] = Expr(name)
          val guard = '{ ${ xExpr }.fields("_kind") == JsString(${ kind }) }

          val mapper = refs(prodMapperIndexByName(name)).asExprOf[JsValueMapper[_]]
          val body = '{ $mapper.fromJson($xExpr) }

          CaseDef(pattern, Some(guard.asTerm), body.asTerm) // case _x: JsObject if _x.fields("_kind") == JsString("Mixed") =>
        }
      }
      val cases2 = cases :+ CaseDef(Wildcard(), None, '{ throw new Exception("no _kind field") }.asTerm) // case _ => throw new Exception("no _kind field")
      
      val expr2 = Match(js.asTerm, cases2)
      expr2.asExprOf[T]

    /**
     * <pre>
     *   def toJson(t: Color): JsValue = t match
     *     case Red => JsString("Red")
     *     ...
     *     case x: Mixed => summon[Mixed].toJson(x)
     * </pre>
     */
    def toJsonImpl(refs: List[Ref], t: Expr[T]): Expr[JsValue] =
      val cases = nameTypes.map { arg =>
        val name = arg._1
        val typ: TypeRepr = arg._2
        val nameExpr = Expr(name)
        if (typ.termSymbol != Symbol.noSymbol) { // case Red => JsString("Red")
          val pattern = Ref(typ.termSymbol)
          val body = '{ JsString(${ nameExpr }) }
          CaseDef(pattern, None, body.asTerm)
        }
        else { // case _x: Mixed => JsObject( summon[Mixed].toJson(_x).fields + ("_kind" -> JsString("Mixed")) )
          val sym = Symbol.newVal(Symbol.spliceOwner, "_x", typ, Flags.EmptyFlags, Symbol.noSymbol)
          val typeTree = TypeTree.of(using typ.asType)
          val bindPattern = Typed(Wildcard(), typeTree)
          val pattern = Bind(sym, bindPattern)
          val xExpr: Expr[Any] = Ref(sym).asExprOf[Any]

          val mapper = refs(prodMapperIndexByName(name)).asExprOf[JsValueMapper[_]].asInstanceOf[Expr[JsValueMapper[Any]]]
          val body = '{ JsObject($mapper.toJson($xExpr).asInstanceOf[JsObject].fields + ("_kind" -> JsString(${ nameExpr }))) }

          CaseDef(pattern, None, body.asTerm)
        }
      }
      val expr = Match(t.asTerm, cases)
      expr.asExprOf[JsValue]

    val expr =
      ValDef.let(Symbol.spliceOwner, prodMappers) { refs =>
        val block = '{
          new JsValueMapper[T]:
            def fromJson(js: JsValue): T = ${ fromJsonImpl(refs, '{js}) }

            def toJson(t: T): JsValue = ${ toJsonImpl(refs, '{t}) }
        }
        block.asTerm
      }
    expr.asExprOf[JsValueMapper[T]]


  private def genProduct[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    import JsValueMapper.*

    // for fields with default values, extract the default value
    val defaultParams: Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      if(comp == Symbol.noSymbol)
        throw new AssertionError("no companionClass found for type:" + TypeRepr.of[T].show(using Printer.TypeReprCode))
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    // '{ new CaseField[t](field, default).apply(jso) }'
    def getField(jso: Expr[JsObject], field: Symbol, THIS: Expr[JsValueMapper[T]]): Term =
      val name = field.name
      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          val summonValue: Option[Expr[JsValueMapper[t]]] = // better way to get this reference
            if(TypeRepr.of[t].widen =:= TypeRepr.of[T].widen) Some(THIS.asInstanceOf[Expr[JsValueMapper[t]]])
            else Expr.summon[JsValueMapper[t]]

          val isOption = TypeRepr.of[t].widen <:< TypeRepr.of[Option[?]]
          summonValue match
            case Some(mapper) =>
              defaultParams.get(name) match
                case Some(deff) =>
                  ' { caseFieldGet[t]($jso, ${Expr(name)}, ${deff.asInstanceOf[Expr[t]]})(using $mapper) }
                case None if isOption == false =>
                  ' { caseFieldGet[t]($jso, ${Expr(name)})(using $mapper) }
                case None if isOption =>
                  ' { caseFieldGet[t]($jso, ${Expr(name)}, None.asInstanceOf[t])(using $mapper) }
                case _ => ???
            case None =>
              report.error(s"No JsValueMapper found, owner: ${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      expr.asTerm

    def isPrimitive(tpe: TypeRepr): Boolean =
      val anyVal = TypeRepr.of[AnyVal]
      tpe <:< anyVal

    // '{ (field.name, summon[JsValueMapper[field.type].toJson(value)]) }
    def getFieldAsKV(value: Expr[T], field: Symbol, THIS: Expr[JsValueMapper[T]]): Expr[(String, JsValue)] = // (String, JsValue)
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
            val isPrim  = isPrimitive(TypeRepr.of[t])
            val summonValue: Option[Expr[JsValueMapper[t]]] =
              if(TypeRepr.of[t].widen =:= TypeRepr.of[T].widen) Some(THIS.asInstanceOf[Expr[JsValueMapper[t]]])
              else Expr.summon[JsValueMapper[t]]
            summonValue match
              case Some(mapper) =>
                // (field.name, mapper.toJson( value.field ))
                val select = Select(value.asTerm, field).asExpr.asInstanceOf[Expr[t]]
                val json =
                  if(isPrim) '{ $mapper.toJson($select) }
                  else '{ if($select == null) JsNull else ${mapper}.toJson($select) }

                '{ ( ${Expr(field.name)}, ${json} ) }
              case None =>
                report.error(s"No JsValueMapper found, owner: ${TypeTree.of[T].show} field:${field.name} type:${TypeTree.of[t].show}")
                '{ (${Expr(field.name)}, ??? ) }

    def buildBeanFrom(jso: Expr[JsObject], THIS: Expr[JsValueMapper[T]]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Term] = tpeSym.caseFields.map( field => getField(jso, field, THIS) )
      val constructor = tpeSym.primaryConstructor

      ValDef.let( Symbol.spliceOwner, terms ) { refs =>
        Apply( Select( New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    def buildJsVal(value: Expr[T], THIS: Expr[JsValueMapper[T]]): Expr[JsValue] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Expr[(String, JsValue)]] = tpeSym.caseFields.map( field => getFieldAsKV(value, field, THIS) )
      val asSeq = Expr.ofSeq(terms)
      '{ JsObject( (${asSeq}).filter(_._2 != JsNull):_* ) }

    val expr: Expr[?] = '{
      new JsValueMapper[T]:
        def fromJson(json: JsValue): T =
          val jso = json.asInstanceOf[JsObject]
          val THIS: JsValueMapper[T] = this
          ${ buildBeanFrom('{jso}, '{THIS}) }
        def toJson(value: T): JsValue =
          val THIS: JsValueMapper[T] = this
          ${ buildJsVal('{value}, '{THIS} ) }
    }

    expr.asInstanceOf[Expr[JsValueMapper[T]]]


