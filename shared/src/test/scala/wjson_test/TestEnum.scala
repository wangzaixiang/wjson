package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.*
import wjson.JsValue.{JsEmptyString, JsObject}

//  enum Weekday:
//    case Mon, Tue, Wed, Thu, Fri, Sat, Sun
//
enum Color(val rgb: Int) derives JsValueMapper:
  case Red extends Color(0xFF0000) // "Red" color
  case Green extends Color(0x00FF00) // "Green"
  case Blue extends Color(0x0000FF) // "Blue"
  case Mixed(override val rgb: Int) extends Color(rgb) // { "_kind": "Mixed", "rgb": 0x123456 }
  case Custom(override val rgb: Int) extends Color(rgb) // { "_kind": "Custom", "rgb": 0x123456 }

object MyMapper extends JsValueMapper[Color]:
  override def fromJson(js: JsValue): Color = js match
    case wjson.JsValue.JsString("Red") => Color.Red
    case wjson.JsValue.JsString("Green") => Color.Green
    case wjson.JsValue.JsString("Blue") => Color.Blue
    case _ => throw new Exception("Unknown color")

  override def toJson(t: Color): JsValue = t match
    case Color.Red => JsString("Red")
    case Color.Green => JsString("Green")
    case Color.Blue => JsString("Blue")
    case _ => throw new Exception("Unknown color")

class TestEnum extends AnyFunSuite {


//  test("basic enum") {
//    assert( Weekday.Mon.toJson == JsString("Mon") )
//    assert( JsString("Mon").convertTo[Weekday] == Weekday.Mon )
//  }

  test("color test") {
    val red: Color = Color.Red
//    given x: JsValueMapper[Color] = JsValueMapper.derived[Color]
//    given x: JsValueMapper[Color] = MyMapper
    val mixed = Color.Mixed(0x123456)
    assert( red.toJson == JsString("Red") )
    assert( JsString("Red").convertTo[Color] == red )

    assert( mixed.toJson == JsObject("_kind" -> "Mixed", "rgb" -> 0x123456) )
    assert( JsObject("_kind" -> "Mixed", "rgb" -> 0x123456).convertTo[Color] == mixed )
  }

}
