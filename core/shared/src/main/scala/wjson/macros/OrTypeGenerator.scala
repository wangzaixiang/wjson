package wjson.macros

import wjson.*
import ADTMappingMacro.*
import scala.quoted.*

// TODO _or_ support
class OrTypeGenerator[T: Type] extends Generator[T]:

  def elementTypes(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    tpe match
      case quotes.reflect.OrType(l, r) => elementTypes(l) ++ elementTypes(r)
      case _ => List(tpe)

  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    val elemenTypeReprs: List[TypeRepr] = elementTypes(TypeRepr.of[T])
    val usingTag = false // TODO 是否使用 _tag_ 字段来区分类型
    val tags: List[String] = Nil // TODO

    if usingTag then
      generateWithTags(elemenTypeReprs, tags, deps)
    else
      generateWithoutTags(elemenTypeReprs, deps)

  private def generateWithTags(using Quotes)(elementTypes: List[quotes.reflect.TypeRepr], tags: List[String], deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    ???

  private def generateWithoutTags(using Quotes)(elementTypes: List[quotes.reflect.TypeRepr], deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*

    // the following code not works when inline it, so extract to a method
    def make[t: Type](mapper: Expr[JsValueMapper[t]], obj: Expr[t]): Expr[JsValue] =
      '{ ${ mapper }.toJson(${ obj }) }

    def toJsonImpl(obj: Expr[T]): Expr[JsValue] =
      val cases: List[CaseDef] = elementTypes.map(_.asType).map:
        case '[t] if TypeRepr.of[t] =:= TypeRepr.of[Null] =>
          CaseDef(Literal(NullConstant()), None, '{ JsNull }.asTerm)
        case '[t] =>
          val dep: Expr[JsValueMapper[t]] = summonJsValueMapper[t](deps).get
          val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[t], Flags.EmptyFlags, Symbol.noSymbol)
          val refSym: Expr[t] = Ref(sym).asExprOf[t]
          val bindPattern = Typed(Wildcard(), TypeTree.of[t])
          val pattern = Bind(sym, bindPattern)
          val body = make[t](dep, refSym).asTerm
          CaseDef(pattern, None, body)

      Match(obj.asTerm, cases).asExprOf[JsValue]

    // JsNull -> null or None
    // JsString, JsBoolean, JsNumber -> string or Some(string), etc.
    // JsArray -> Array or List
    // JsObject -> Map or case class
    def fromNull(): Expr[T] =
      if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Null]) then
        '{ null.asInstanceOf[T] }
      else if elementTypes.exists(tpe => tpe <:< TypeRepr.of[Option[?]]) then
        '{ None.asInstanceOf[T] }
      else '{ throw new RuntimeException("No null value allowed") }

    def fromString(value: Expr[String]): Expr[T] =
      if elementTypes.exists(tpe => tpe =:= TypeRepr.of[String]) then
        '{ ${ value }.asInstanceOf[T] }
      else '{ throw new RuntimeException("No string value allowed") }

    def fromLong(value: Expr[Long]): Expr[T] =
      if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Long]) then
        '{ ${ value }.asInstanceOf[T] }
      else if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Int]) then
        '{ ${ value }.toInt.asInstanceOf[T] }
      else '{ throw new RuntimeException("No long value allowed") }

    def fromDouble(value: Expr[Double]): Expr[T] =
      if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Double]) then
        '{ ${ value }.asInstanceOf[T] }
      else if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Float]) then
        '{ ${ value }.toFloat.asInstanceOf[T] }
      else '{ throw new RuntimeException("No double value allowed") }

    def fromBoolean(value: Expr[Boolean]): Expr[T] =
      if elementTypes.exists(tpe => tpe =:= TypeRepr.of[Boolean]) then
        '{ ${ value }.asInstanceOf[T] }
      else '{ throw new RuntimeException("No boolean value allowed") }

    def fromArray(js: Expr[JsArray]): Expr[T] = '{ throw new RuntimeException("fromArray is not implemented yet") }

    def fromObject(js: Expr[JsObject]): Expr[T] = '{ throw new RuntimeException("fromObject is not implmented yet") }

    '{
      new JsValueMapper[T]:
        override def fromJson(js: JsValue): T = // ${fromJsonImpl('{js}) }
          js match
            case JsNull => ${ fromNull() }
            case JsString(s) => ${ fromString('s) }
            case JsNumber(l: Long) => ${ fromLong('l) }
            case JsNumber(d: Double) => ${ fromDouble('d) }
            case JsBoolean(b) => ${ fromBoolean('b) }
            case s: JsArray => ${ fromArray('s) }
            case s: JsObject => ${ fromObject('s) }

        override def toJson(x: T): JsValue = ${ toJsonImpl('{ x }) }
    }
