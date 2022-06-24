package wjson

import org.scalatest.funsuite.AnyFunSuite
import JsVal.*

class TestJsonMap extends AnyFunSuite {

  test("Case Class Mapping") {

    case class User(name: String, age: Int)
    val mapper = summon[JsValMapper[User]]

    val user = User("John", 30)
    val js = mapper.toJson(user)
    assert( js == json"""{ "name": "John", "age": 30 }""")

  }

}
