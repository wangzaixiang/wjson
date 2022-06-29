package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.*

import scala.collection.SortedSet

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

    assert( JsBoolean(true).convertTo[Boolean] == true)
    assert( JsBoolean(false).convertTo[Boolean] == false)
    assert( true.toJson == JsBoolean(true))
    assert( false.toJson == JsBoolean(false))

    assert( JsNumber(1).convertTo[Byte] == 1)
    assert( 1.toByte.toJson == JsNumber(1) )
    assert( JsNumber(1).convertTo[Short] == 1)
    assert( 1.toShort.toJson == JsNumber(1) )

    assert( JsNumber(1).convertTo[Int] == 1)
    assert( 1.toJson == JsNumber(1) )
    assert( JsNumber(1).convertTo[Long] == 1)
    assert( 1L.toJson == JsNumber(1))

    assert( 1.1f.toJson == JsNumber(1.1f))
    assert( 1.1.toJson == JsNumber(1.1))
    assert( JsNumber(1.1).convertTo[Float] == 1.1f)
    assert( JsNumber(1.1).convertTo[Double] == 1.1)

    assert( BigDecimal(1.5).toJson == JsNumber(1.5))
    assert( JsNumber(1.5).convertTo[BigDecimal] == BigDecimal(1.5) )

    assert("abc".toJson == JsString("abc"))
    assert(JsString("abc").convertTo[String] == "abc")

  }

  test("Collection Mappers and coversions") {

    // List
    assert( List(1,2,3).toJson == JsArray(1,2,3) )
    assert( JsArray(1,2,3).convertTo[List[Int]] == List(1,2,3))
    assert( JsArray(1,2,3).convertTo[List[Short]] == List(1.toShort,2.toShort,3.toShort))
    assert( (List(1,2,3):JsValue) == JsArray(1,2,3))

    // Seq
    assert( Seq(1,2,3).toJson == JsArray(1,2,3) )
    assert( JsArray(1,2,3).convertTo[Seq[Int]] == Seq(1,2,3))
    assert( (Seq(1,2,3):JsValue) == JsArray(1,2,3))

    // SortedSet
    assert( SortedSet(3,2,1).toJson == JsArray(1,2,3) )
    assert( JsArray(3,2,1).convertTo[SortedSet[Int]] == SortedSet(1,2,3))
    assert( (SortedSet(3,2,1):JsValue) == JsArray(1,2,3))

    // Map
    assert( Map("a"->1,"b"->2,"c"->3).toJson == JsObject("a"->1,"b"->2,"c"->3))
    assert( JsObject("a"->1,"b"->2,"c"->3).convertTo[Map[String,Int]] == Map("a"->1,"b"->2,"c"->3))
    assert( (Map("a"->1,"b"->2,"c"->3):JsValue) == JsObject("a"->1,"b"->2,"c"->3))
  }

  test("Option Mappers"){
    val some1: Option[Int] = Some(1)
    val none: Option[Int] = None
    assert( some1.toJson == JsNumber(1))
    assert( JsNumber(1).convertTo[Option[Int]] == Some(1))
    assert( none.toJson == JsNull)
    assert( JsNull.convertTo[Option[Int]] == None)
  }

  test("case class mappers"){

    case class User1(name: String, age: Int, address: Address1) derives JsValueMapper
    case class Address1(street: String, city: String) derives JsValueMapper

    val user = User1("John", 30, Address1("Main St", "New York"))
    assert( user.toJson == JsObject("name"->"John","age"->30,
      "address"->JsObject("street"->"Main St","city"->"New York")))
    assert( json"{name:'John',age:30,address:{street:'Main St',city:'New York'}}".convertTo[User1] == user)

  }

  test("Self Reference Case Class") {
    case class User2(name: String, age: Int, owner: User2 = null) derives JsValueMapper

    val user = User2("John", 30, null)
    val child = User2("Mary", 10, user)

    val js = child.toJson

    assert( js == JsObject("name"->"Mary","age"->10,
      "owner"->JsObject("name"->"John","age"->30)))
    assert( js.convertTo[User2] == child)

  }

  // failed, cross reference not support yet
  /*
  test("Cross Reference Case Class") {
    case class Teacher(name: String, age: Int, student: Student = null)
    case class Student(name: String, age: Int /*, teacher: Teacher*/)

    val teacher = Teacher("John", 30, null)
    val student = Student("Mary", 10 /*, teacher*/)

    val js1 = teacher.toJson
    val js2 = student.toJson

    println(s"js1 = $js1 js2 = $js2")
  }
  */

}