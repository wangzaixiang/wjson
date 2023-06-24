package wjson.macros

import wjson.*
import ADTMappingMacro.*
import scala.quoted.*
import scala.deriving.*

class SumGenerator[T: Type] extends Generator[T]:

  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    Expr.summon[Mirror.Of[T]].get match
      case '{ $m: Mirror.SumOf[T] {type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames} } =>
        generate[elemTypes, elemNames](deps)

  private def generate[elemTypes: Type, elemNames: Type](using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    val names = extractElemLabels[elemNames]
    val typeReprs = extractElemTypes[elemTypes]

    val nameTypes: List[(String, TypeRepr)] = names.zip(typeReprs)

    /**
     * <pre>
     * def fromJson(json: JsValue): Color = json match
     * case JsString("Red") => Red
     * ...
     * case x:JsObject if x.fields("_kind") == "Mixed" => summon[Mixed].fromJson(json)
     * </pre>
     */
    def fromJsonImpl(js: Expr[JsValue]): Expr[T] =
      val cases = nameTypes.map { arg =>
        val name: String = arg._1
        val tpe: TypeRepr = arg._2
        if (tpe.termSymbol != Symbol.noSymbol) { // case Red of single value
          val selector = Ref(Symbol.requiredModule("wjson.JsValue.JsString"))
          val tree = Unapply(Select.unique(selector, "unapply"), Nil, List(Literal(StringConstant(name))))
          val tpt = Inferred(TypeRepr.of[JsString])
          val pattern = TypedOrTest(tree, tpt)
          CaseDef(pattern, None, Ref.term(tpe.asInstanceOf[TermRef])) // case JsString("Red") => Red
        }
        else { // case Mixed(rgb) of a case class
          val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[JsValue.JsObject], Flags.EmptyFlags, Symbol.noSymbol)
          val bindPattern = Typed(Wildcard(), TypeTree.of[JsValue.JsObject])
          val pattern = Bind(sym, bindPattern)
          val xExpr = Ref(sym).asExprOf[JsValue.JsObject]
          val kind: Expr[String] = Expr(name)
          val guard = '{ ${ xExpr }.field("_kind") == JsString(${ kind }) }

          val mapper = summonJsValueMapper(using quotes)(tpe, deps).get
          val body = '{ ${ mapper }.fromJson(${ xExpr }) }

          CaseDef(pattern, Some(guard.asTerm), body.asTerm) // case _x: JsObject if _x.fields("_kind") == JsString("Mixed") =>
        }
      }
      val cases2 = cases :+ CaseDef(Wildcard(), None, '{ throw new Exception("no _kind field") }.asTerm) // case _ => throw new Exception("no _kind field")

      val expr2 = Match(js.asTerm, cases2)
      expr2.asExprOf[T]

    /**
     * <pre>
     * def toJson(t: Color): JsValue = t match
     * case Red => JsString("Red")
     * ...
     * case x: Mixed => summon[Mixed].toJson(x)
     * </pre>
     */
    def toJsonImpl(t: Expr[T]): Expr[JsValue] =
      val cases = nameTypes.map { arg =>
        val name = arg._1
        val typ: TypeRepr = arg._2
        val nameExpr = Expr(name)
        if (typ.termSymbol != Symbol.noSymbol) { // case Red => JsString("Red")
          val pattern = Ref.term(typ.asInstanceOf[TermRef])
          val body = '{ JsString(${ nameExpr }) }
          CaseDef(pattern, None, body.asTerm)
        }
        else { // case _x: Mixed => JsObject( summon[Mixed].toJson(_x).fields + ("_kind" -> JsString("Mixed")) )
          val sym = Symbol.newVal(Symbol.spliceOwner, "_x", typ, Flags.EmptyFlags, Symbol.noSymbol)
          val typeTree = TypeTree.of(using typ.asType)
          val bindPattern = Typed(Wildcard(), typeTree)
          val pattern = Bind(sym, bindPattern)

          val body = typ.asType match
            case '[t] =>
              val xExpr = Ref(sym).asExprOf[t]
              val mapper = summonJsValueMapper[t](deps).get
              '{ JsObject("_kind" -> JsString(${ nameExpr })) ++ ${ mapper }.toJson(${ xExpr }).asInstanceOf[JsObject] }
          CaseDef(pattern, None, body.asTerm)
        }
      }
      val expr = Match(t.asTerm, cases)
      expr.asExprOf[JsValue]

    val expr = '{
      new JsValueMapper[T]:
        def fromJson(js: JsValue): T = ${ fromJsonImpl('{ js }) }

        def toJson(t: T): JsValue = ${ toJsonImpl('{ t }) }
    }
    expr.asExprOf[JsValueMapper[T]]

