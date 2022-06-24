package wjson

import org.scalatest.funsuite.AnyFunSuite
import JsVal.*

class TestJsonInterpolation extends AnyFunSuite {

  test("json interpolation") {
    val js1 = json"""
    {
      "name": "John",
      "age": 30,
    }
    """
    assert( js1 == JsObj(
      "name" -> JsString("John"),
      "age" -> JsNumber(30)
    ) )
  }

  test("json unapply") {
    val js = JsObj(
      "name" -> JsString("John"),
      "age" -> JsNumber(30)
    )
    js match {
      case json"""{name: $name, age: $age}""" =>
        assert( name == JsString("John") )
        assert( age == JsNumber(30) )
    }
  }
}
