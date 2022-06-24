package wjson

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}
import JsVal.*

object JsValMapperMacro:

  case class User(name: String, age: Int)

  inline def summonAll[T <: Tuple]: List[JsValMapper[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[JsValMapper[t]] :: summonAll[ts]

//  inline def derived[T](using m: Mirror.ProductOf[T]): JsValMapper[T] =
//    new JsValMapper[T] {
//      def fromJson(json: JsVal): T =
//        val mappers: List[JsValMapper[Any]] = summonAll[m.MirroredElemTypes].asInstanceOf[List[JsValMapper[Any]]]
//        val jso = json.asInstanceOf[JsVal.JsObj]
//        val fields = mappers.zipWithIndex.map {
//          case (mapper, i) =>
//            (i, mapper.fromJson(jso.fields(i)))
//        }
//        val prod = ???
//        m.fromProduct(prod)
//      def toJson(value: T): JsVal =
//        val prod = value.asInstanceOf[Product]
//        val mappers: List[JsValMapper[Any]] = summonAll[m.MirroredElemTypes].asInstanceOf[List[JsValMapper[Any]]]
//        val fields = mappers.zipWithIndex.map { e =>
//          val (mapper, i) = e
//          prod.productElementName(i) -> mapper.toJson(prod.productElement(i))
//        }.toMap
//        JsVal.JsObj(fields)
//    }

  def generate[T: Type](using Quotes): Expr[JsValMapper[T]] = // TODO ???
    import quotes.reflect.*

    val defaultParams: Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    def getField(jso: Expr[JsVal], field: Symbol): Term =
      val name = field.name
      val defaultExpr: Expr[Option[Any]] = defaultParams.get(name) match
        case Some(deff) => '{ Some($deff) }
        case None => '{ None }

      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          Expr.summon[JsValMapper[t]] match
            case Some(mapper) =>
              '{ new CaseField[t](${Expr(name)}, ${defaultExpr}.asInstanceOf[Option[t]])(using $mapper).apply($jso.asInstanceOf[JsObj]) }
            case None =>
              report.error(s"No JsValMapper found, owner: ${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      expr.asTerm

    def isPrimitive(tpe: TypeRepr): Boolean =
      val anyVal = TypeRepr.of[AnyVal]
      tpe <:< anyVal

    def getField2(value: Expr[T], field: Symbol): Expr[(String, JsVal)] = // (String, JsVal)
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
            val isPrim  = isPrimitive(TypeRepr.of[t])
            Expr.summon[JsValMapper[t]] match
              case Some(mapper) =>
                // (field.name, mapper.toJson( value.field ))
                val select = Select(value.asTerm, field).asExpr.asInstanceOf[Expr[t]]
                val json =
                  if(isPrim) '{ $mapper.toJson($select) }
                  else '{ if($select == null) JsNull else ${mapper}.toJson($select) }

                '{ ( ${Expr(field.name)}, ${json} ) }
              case None =>
                report.error(s"No JsValMapper found, owner: ${TypeTree.of[T].show} field:${field.name} type:${TypeTree.of[t].show}")
                '{ (${Expr(field.name)}, ??? ) }

    def buildBeanFrom(js: Expr[JsVal]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Term] = tpeSym.caseFields.map( field => getField(js, field) )
      val companion = tpeSym.companionModule
      val applyMethod = companion.memberMethod("apply").apply(0)
      ValDef.let( Symbol.spliceOwner, terms ) { refs =>
        Apply( Select(Ref(companion), applyMethod), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    def buildJsVal(value: Expr[T]): Expr[JsVal] =
      // JsObj( (String, JsVal)* )
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Expr[(String, JsVal)]] = tpeSym.caseFields.map( field => getField2(value, field) )
      val asSeq = Expr.ofSeq(terms)
      '{ JsObj( ${asSeq}:_* ) }

    val expr = '{
      new JsValMapper[T]:
        def fromJson(json: JsVal): T = ${ buildBeanFrom('{json}) }
        def toJson(value: T): JsVal = ${ buildJsVal('{value} ) }
    }

//    val exp =  '{
//       new JsValMapper[User]:
//         def fromJson(jsVal: JsVal): User = {
//           val obj = jsVal.asInstanceOf[JsVal.JsObj]
//           val name = obj.fields.get("name") match {
//               case Some(js) => summon[JsValMapper[String]].fromJson(js)
//               case None => ??? // or default value
//             }
//           val age = {
//               obj.fields.get("age") match {
//                 case Some(js) => summon[JsValMapper[Int]].fromJson(js)
//                 case None => ???
//               }
//           }
//
//           val age = summon[JsValMapper[Int]].from(obj.fields["age"])
//           new User(name, age)
//         }
//         def toJson(user: User): JsVal = {
//           val name = "name" -> {
//             if (user.name == null) JsNull
//             else summon[JsValMapper[String]].to(user.name)
//           }
//           val age = "age" -> {
//             summon[JsValMapper[Int]].to(user.age)
//           }
//           JsVal.JsObj( name, age )
//         }
//     }

    // println("generated type " + expr.show)

    expr.asInstanceOf[Expr[JsValMapper[T]]]


