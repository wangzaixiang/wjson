package wjson

import org.scalatest.funsuite.AnyFunSuite

import java.nio.DoubleBuffer

class TestJsValue extends AnyFunSuite {

  test("Json Parser") {
    val jsonStr = """{"name":"John","age":30,"cars":["Ford","BMW","Fiat"],"city":"New York"}"""
    val jsval = jsonStr.parseJson

    assert(jsval == JsObject(
      "name" -> "John",
      "age" -> 30,
      "cars" -> List("Ford", "BMW", "Fiat"),
      "city" -> "New York"
    ))

    val x: JsValue = List("a", "b", "c")
    assert(x == List("a", "b", "c").toJson)
  }

  test("Primitive Type Mappers") {

    assert( JsBoolean(true).to[Boolean] == true)
    assert( JsBoolean(false).to[Boolean] == false)
    assert( true.toJson == JsBoolean(true))
    assert( false.toJson == JsBoolean(false))

    assert( JsNumber(1).to[Byte] == 1)
    assert( 1.toByte.toJson == JsNumber(1) )
    assert( JsNumber(1).to[Short] == 1)
    assert( 1.toShort.toJson == JsNumber(1) )

    assert( JsNumber(1).to[Int] == 1)
    assert( 1.toJson == JsNumber(1) )
    assert( JsNumber(1).to[Long] == 1)
    assert( 1L.toJson == JsNumber(1))

    assert( 1.1f.toJson == JsNumber(1.1f))
    assert( 1.1.toJson == JsNumber(1.1))
    assert( JsNumber(1.1).to[Float] == 1.1f)
    assert( JsNumber(1.1).to[Double] == 1.1)

    assert( BigDecimal(1.5).toJson == JsNumber(1.5))
    assert( JsNumber(1.5).to[BigDecimal] == BigDecimal(1.5) )

    assert("abc".toJson == JsString("abc"))
    assert(JsString("abc").to[String] == "abc")

  }

  test("Collection Mappers") {

      assert( List(1,2,3).toJson == JsArray(1,2,3) )
      assert( JsArray(1,2,3).to[List[Int]] == List(1,2,3))
      assert( JsArray(1,2,3).to[List[Short]] == List(1.toShort,2.toShort,3.toShort))

      assert( Seq(1,2,3).toJson == JsArray(1,2,3) )
      assert( JsArray(1,2,3).to[Seq[Int]] == Seq(1,2,3))

      assert( Map("a"->1,"b"->2,"c"->3).toJson == JsObject("a"->1,"b"->2,"c"->3))
      assert( JsObject("a"->1,"b"->2,"c"->3).to[Map[String,Int]] == Map("a"->1,"b"->2,"c"->3))
  }

  test("Option Mappers"){
    val some1: Option[Int] = Some(1)
    val none: Option[Int] = None
    assert( some1.toJson == JsNumber(1))
    assert( JsNumber(1).to[Option[Int]] == Some(1))
    assert( none.toJson == JsNull)
    assert( JsNull.to[Option[Int]] == None)
  }

  test("case class mappers"){

    case class User(name: String, age: Int, address: Address)
    case class Address(street: String, city: String)

    val user = User("John", 30, Address("Main St", "New York"))
    assert( user.toJson == JsObject("name"->"John","age"->30,
      "address"->JsObject("street"->"Main St","city"->"New York")))
    assert( json"{name:'John',age:30,address:{street:'Main St',city:'New York'}}".to[User] == user)

  }

}