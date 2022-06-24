package wjson

import org.scalatest.funsuite.AnyFunSuite
import JsVal.*

class TestJsonInterpolation extends AnyFunSuite {

  test("json interpolation") {
    val age = 30
    val js1 = json"""
    {
      name: "John",
      age: $age,
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
      "age" -> JsNumber(30),
      "address" -> JsObj(
        "street" -> JsString("Main St"),
        "city" -> JsString("New York"),
        "state" -> JsString("NY")
      )
    )
    js match {
      case json"""{name: $name, address: { state: "NY" } }""" =>
        assert( name == JsString("John") )
//        assert( age == JsNumber(30) )
    }
  }
}
