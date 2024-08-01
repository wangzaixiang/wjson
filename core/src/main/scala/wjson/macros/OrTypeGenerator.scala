package wjson.macros

import wjson.*
import ADTMappingMacro.*

import scala.annotation.tailrec
import scala.quoted.*

/**
 * A | B | C maps to { _ortype: 1|2|3, value: ? }
 * 1. when there is 2+ types mapping to JsObject, a tag is added
 * 2. can't using T | Option[T] because they have the  same tag
 * 3. can't using Option[T] | Null together
 */
class OrTypeGenerator[T: Type] extends Generator[T]:

  def elementTypes(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    tpe match
      case quotes.reflect.OrType(l, r) => elementTypes(l) ++ elementTypes(r)
      case _ => List(tpe)

  // given a unique tag for each ElementType
  // Option[T] and T have the same tag
  @tailrec
  private def tagOf(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*
    if tpe =:= TypeRepr.of[Null] then "Null"
    else if tpe <:< TypeRepr.of[Option[?]] then
        tpe.asType match
            case '[Option[t]] => tagOf(TypeRepr.of[t])
        else tpe.typeSymbol.fullName

  @tailrec
  private def isJsonPrimitive(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*
    if tpe =:= TypeRepr.of[String] || tpe =:= TypeRepr.of[Int] || tpe =:= TypeRepr.of[Long] ||
       tpe =:= TypeRepr.of[Double] || tpe =:= TypeRepr.of[Boolean] || tpe =:= TypeRepr.of[Null] then true
    else if tpe <:< TypeRepr.of[Option[?]] then
      tpe.asType match
        case '[Option[t]] => isJsonPrimitive(TypeRepr.of[t])
    else false

  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*

    case class TagInfo(tpe: TypeRepr, tag: String, isOption: Boolean, isJsonPrimitive: Boolean)

    val elemTpes: List[TypeRepr] = elementTypes(TypeRepr.of[T])
    val tags: List[TagInfo] = elemTpes.map(tpe => TagInfo(tpe, tagOf(tpe), tpe <:< TypeRepr.of[Option[?]], isJsonPrimitive(tpe)))
    val tagsByTpe: Map[TypeRepr, TagInfo] = tags.map( tag => tag.tpe -> tag).toMap
    val hasMultiTags: Boolean = tags.filterNot(tag => tag.isJsonPrimitive).size > 1

    tags.groupBy(_.tag).filter(_._2.size > 1).foreach { case (tagName, tags) =>
      val tagInfos = tags.map(tag => s"${tag.tpe.show}").mkString("[", ",", "]")
      report.error(s"Duplicate tag $tagName for ${TypeRepr.of[T].show} . $tagInfos")
    }

    if tags.exists(_.isOption) && tags.exists(_.tag == "Null") then
      report.error(s"Option[T] can't used with Null type for ${TypeRepr.of[T].show}")

    def toJsonWrapTagIfNeeded[t: Type](mapper: Expr[JsValueMapper[t]], obj: Expr[t]): Expr[JsValue] =
      val tag = tagsByTpe(TypeRepr.of[t])
      val tagExpr: Expr[String] = Expr(tag.tag)
      val tpe = TypeRepr.of[t]
      if tpe =:= TypeRepr.of[String] then '{ JsString(${ obj.asExprOf[String] }) }
      else if tpe =:= TypeRepr.of[Int] then '{ JsNumber(${ obj.asExprOf[Int] }) }
      else if tpe =:= TypeRepr.of[Long] then '{ JsNumber(${ obj.asExprOf[Long] }) }
      else if tpe =:= TypeRepr.of[Double] then '{ JsNumber(${ obj.asExprOf[Double] }) }
      else if tpe =:= TypeRepr.of[Boolean] then '{ JsBoolean(${ obj.asExprOf[Boolean] }) }
      else if tpe =:= TypeRepr.of[Null] then '{ JsNull }
      else
        val simple = '{ ${mapper}.toJson( ${obj}) }
        if tag.isJsonPrimitive then simple
        else if hasMultiTags then
        '{ JsObject( "_type" ->  JsString($tagExpr), "_value" -> ${ mapper }.toJson(${ obj }) ) }
        else simple

    def toJsonImpl(obj: Expr[T], hasMultiTag: Boolean): Expr[JsValue] =
      val matchNone: List[CaseDef] = tags.exists(_.isOption) match
        case true =>
           // TODO explict None match will report match may not be exhaustive
           // val noneObj = Ref( Symbol.requiredModule("scala.None") )
           // Some(CaseDef(noneObj, None, '{ JsNull }.asTerm))
           List( CaseDef( Wildcard(), None, '{ JsNull }.asTerm) )
        case false => Nil

      val cases: List[CaseDef] = elemTpes.map(_.asType).map:
        case '[t] if TypeRepr.of[t] =:= TypeRepr.of[Null] =>
          CaseDef(Literal(NullConstant()), None, '{ JsNull }.asTerm)
        case '[t] =>
          val dep: Expr[JsValueMapper[t]] = summonJsValueMapper[t](deps).get
          val symOut = Symbol.newVal(Symbol.spliceOwner, "x1", TypeRepr.of[t], Flags.EmptyFlags, Symbol.noSymbol)
          val pattern =
            if TypeRepr.of[t] <:< TypeRepr.of[Option[?]] then  // x1: Some(x2: t1) =>
              Type.of[t] match
                case '[Option[t1]] =>
                    val symInner = Symbol.newVal(Symbol.spliceOwner, "x2", TypeRepr.of[t1], Flags.EmptyFlags, Symbol.noSymbol)
                    val func = Select.unique( Ref(Symbol.requiredModule("scala.Some")), "unapply" )
                    val term = TypeApply( func, List( TypeTree.of[Any] ) )
                    val innerBind = Bind(symInner, Typed(Wildcard(), TypeTree.of[t1]))
                    val unapply = Unapply( term, implicits = Nil, patterns = List( innerBind ) )
                    val typedOrTest = TypedOrTest( unapply, TypeTree.of[Some[Any]] )
                    Bind( symOut, typedOrTest)
            else // x1: t =>
              Bind(symOut, Typed(Wildcard(), TypeTree.of[t]) )
          val body = toJsonWrapTagIfNeeded[t](dep, Ref(symOut).asExprOf[t]).asTerm  // ${mapper}.toJson(${obj}).asInstanceOf[JsValue]
          CaseDef(pattern, None, body)

      val result = Match(obj.asTerm, cases ++ matchNone ).asExprOf[JsValue]
      result

    def fromNull(): Expr[T] =
      if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Null]) then
        '{ null.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe <:< TypeRepr.of[Option[?]]) then
        '{ None.asInstanceOf[T] }
      else '{ throw new RuntimeException("No null value allowed") }

    def fromString(value: Expr[String]): Expr[T] =
      if elemTpes.exists(tpe => tpe =:= TypeRepr.of[String]) then
        '{ ${ value }.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[String]]) then
        '{ Some(${ value }).asInstanceOf[T] }
      else '{ throw new RuntimeException("No string value allowed") }

    def fromLong(value: Expr[Long]): Expr[T] =
      if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Long]) then '{ ${ value }.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Int]) then '{ ${ value }.toInt.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[Long]]) then '{ Some(${ value }).asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[Int]]) then '{ Some(${value}.toInt).asInstanceOf[T] }
      else '{ throw new RuntimeException("No long value allowed") }

    def fromDouble(value: Expr[Double]): Expr[T] =
      if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Double]) then '{ ${ value }.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Float]) then '{ ${ value }.toFloat.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[Double]]) then '{ Some(${ value }).asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[Float]]) then '{ Some(${ value }.toFloat).asInstanceOf[T] }
      else '{ throw new RuntimeException("No double value allowed") }

    def fromBoolean(value: Expr[Boolean]): Expr[T] =
      if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Boolean]) then '{ ${ value }.asInstanceOf[T] }
      else if elemTpes.exists(tpe => tpe =:= TypeRepr.of[Option[Boolean]]) then '{ Some(${ value }).asInstanceOf[T] }
      else '{ throw new RuntimeException("No boolean value allowed") }
      
    def fromJsArray(array: Expr[JsArray]): Expr[T] =
      if hasMultiTags then
        '{ throw new RuntimeException("No array type allowed") }
      else
        tags.find(tag => !tag.isJsonPrimitive) match
          case Some(tag: TagInfo) =>
            tag.tpe.asType match
                case '[t] =>
                    val dep: Expr[JsValueMapper[t]] = summonJsValueMapper[t](deps).get
                    '{ ${dep}.fromJson(${array}) }.asExprOf[T]
          case None =>
            '{ throw new RuntimeException("No object type allowed") }

    // maybe test tag first
    def fromJsObject(jso: Expr[JsObject]): Expr[T] =
      if hasMultiTags then
        '{
          assert($jso.contains("_type"), "required _type field in Json")
          assert($jso.contains("_value"), "required _value field in Json")
          assert($jso.field("_type").isInstanceOf[JsString], "required _type field in Json")

          val tag = $jso.field("_type").asInstanceOf[JsString].value
          val value = $jso.field("_value")
          ${ fromJsObjectByTag('{tag }, '{value}) }
        }
      else  // at most 1 non-primitive type
        tags.find(tag => !tag.isJsonPrimitive) match
          case Some(tag: TagInfo) =>
            fromJsObjectByTag(Expr(tag.tag), jso)
          case None =>
            '{ throw new RuntimeException("No object type allowed") }

    def fromJsObjectByTag(tagName: Expr[String | Null], value: Expr[JsValue]): Expr[T] =
      val cases: List[CaseDef] = elemTpes.filterNot(tpe => tpe =:= TypeRepr.of[Null]).map(_.asType) flatMap :
        case '[t] =>
          val tag = tagsByTpe(TypeRepr.of[t])
          if tag.isJsonPrimitive then
            None
          else
            val dep: Expr[JsValueMapper[t]] = summonJsValueMapper[t](deps) match
              case Some(x) => x
              case None => throw new RuntimeException(s"Cannot find JsValueMapper for ${tagOf(TypeRepr.of[t])}")

            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[t], Flags.EmptyFlags, Symbol.noSymbol)
            val refSym: Expr[t] = Ref(sym).asExprOf[t]
            val bindPattern = Typed(Wildcard(), TypeTree.of[t])
            val pattern = Bind(sym, bindPattern)
            val body = '{ ${ dep }.fromJson(${ value }).asInstanceOf[T] }
            Some(CaseDef(Literal(StringConstant(tag.tag)), None, body.asTerm))

      Match(tagName.asTerm, cases).asExprOf[T]


    val typeInfo = Expr(TypeRepr.of[T].show(using Printer.TypeReprCode))
    '{
      new JsValueMapper[T]:
        val TYPE = ${typeInfo}
        override def fromJson(js: JsValue): T = // ${fromJsonImpl('{js}) }
          js match
            case JsNull => ${ fromNull() }
            case JsString(str) => ${ fromString('{str}) }
            case JsNumber(l: Long) => ${ fromLong('l) }
            case JsNumber(d: Double) => ${ fromDouble('d) }
            case JsBoolean(b) => ${ fromBoolean('b) }
            case arr: JsArray => ${ fromJsArray('{arr}) }
            case s: JsObject => ${fromJsObject('{s} ) }

        override def toJson(x: T): JsValue = ${ toJsonImpl('{ x }, hasMultiTags) }
    }

