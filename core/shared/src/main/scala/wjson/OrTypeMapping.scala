package wjson

import wjson.*

import scala.quoted.*

object OrTypeMapping:

  inline def gen[T: JsValueMapper, U: JsValueMapper]: JsValueMapper[T|U] =
    ${ genOrTypeMapper[T, U] }

  def genOrTypeMapper[T: Type, U: Type](using Quotes): Expr[JsValueMapper[T|U]] =
    import quotes.reflect.*
    val t1 = Type.of[T]
    val u1 = Type.of[U]
    println(s"genOrType: [$t1, $u1]")
    '{
//      val obj = new JsValueMapper[String|Int]:
//        override def fromJson(js: JsValue): String|Int = js match
//          case JsString(s) => s
//          case JsNumber(n: Long) => n.toInt
//          case _ => throw new IllegalArgumentException("Expected JsString or JsNumber, but found: " + js)
//
//        override def toJson(t: String|Int): JsValue = t match
//          case x: String => JsString(x)
//          case y: Int => JsNumber(y)

      null
    }



