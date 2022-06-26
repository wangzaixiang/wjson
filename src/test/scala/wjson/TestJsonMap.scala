package wjson

import org.scalatest.funsuite.AnyFunSuite

class TestJsonMap extends AnyFunSuite {

  test("Case Class Mapping") {

//    given JsValMapper[BigDecimal] with
//      def fromJson(j: JsVal): BigDecimal = BigDecimal(j.asInstanceOf[JsNumber].value)
//      def toJson(v: BigDecimal): JsVal = JsNumber(v.doubleValue)

    case class Address(street: String, city: String, state: String, zip: String)

    case class User(name: String, age: Int, address: Address)

    val user = User("John", 30, Address("123 Main St", "Anytown", "NY", "12345"))
    val js2 = user.toJson
    println(s"js2 = $js2")
    assert( js2 ==
      json"""{ "name": "John", "age": 30,
            "address": { "street": "123 Main St", "city": "Anytown", "state": "NY", "zip": "12345" }
            }""" )

    val user2 = js2.to[User]
    assert(user2 == user)
    println(s"user2= $user2")

  }

}
