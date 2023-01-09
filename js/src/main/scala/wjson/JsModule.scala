package wjson

import wjson.JsValue.{JsBoolean, JsNumber}

import scala.collection.mutable
import scala.scalajs.js.annotation.*
import scala.scalajs.js

@js.native @JSGlobal
object JSON extends js.Object:
  def stringify(value: js.Any): String = js.native

@JSExportTopLevel("wjson")
object JsModule {

  def main(args: Array[String]): Unit = {
    val json = """{name: 'wangzx', age: 20, email:'www@qq.com'}"""
    val pattern = "{name: n@_, other @ _*}"
    rejsonMatch(json, pattern) match
      case x => println("matched:" + JSON.stringify(x) )
      case null => println("not matched")
  }

  @JSExport
  def rejsonMatch(json: String, pattern: String): js.Any =
    try
      val jsv = new JsonParser(ParserInput(json), true).parseJsValue()
      val jsp = JsPatternParser.parseRejson(pattern)

      RejsonMatcher(jsp).unapplyAsMap(jsv) match
        case Some(result) =>
          mapAsJsDictionary(result)
        case _ => "matched failed"
    catch
      case ex: Exception => s"something wrong: ${ex.getMessage}"

  def mapAsJsDictionary(value: Map[String, Any]): js.Dictionary[js.Any] =
    js.Dictionary( value.toSeq.map {
      case (k,v) => k -> mapAsJsValue(v)
    }:_* )

  private def mapAsJsValue(value: Any): js.Any =
    value match
      case x: Map[_, _] => mapAsJsDictionary(x.asInstanceOf[Map[String, Any]])
      case JsNumber(x: Double) => x
      case JsNumber(x: Long) => x.toDouble
      case x: JsString => x.value
      case x: JsBoolean => x.value
      case x: JsArray => js.Array( x.elements.map(mapAsJsValue):_* )
      case x: JsObject => js.Dictionary( x.fields.toSeq.map { case (k,v) => k -> mapAsJsValue(v) }:_* )
      case x: js.Object => x
      case null => null
}
