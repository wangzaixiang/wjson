package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.{given, *}

class TestJsonInterpolation extends AnyFunSuite {

  test("json interpolation") {
    val age = 30
    val js1 = json"""
    {
      name: "John",
      age: $age,
    }
    """
    assert( js1 == JsObject(
      "name" -> JsString("John"),
      "age" -> JsNumber(30)
    ) )
  }

  test("json unapply") {
    val js = JsObject(
      "name" -> JsString("John"),
      "age" -> JsNumber(30),
      "address" -> JsObject(
        "street" -> JsString("Main St"),
        "city" -> JsString("New York"),
        "state" -> JsString("NY")
      )
    )
    js match {
      case json"""{name: $name, age:$age, address: { state: "NY", city: $city } }""" =>
        assert( name == JsString("John") )
        assert( age == JsNumber(30) )
        assert( city == JsString("New York") )
    }
  }
}
