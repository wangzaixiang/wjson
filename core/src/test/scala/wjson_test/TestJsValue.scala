package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsValue.JsNumber
import wjson.{*, given}

import scala.collection.SortedSet
import scala.language.implicitConversions

class TestJsValue extends AnyFunSuite {

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
//    assert( (Map("a"->1,"b"->2,"c"->3):JsValue) == JsObject("a"->1,"b"->2,"c"->3)) // the code fails

  }

  test("Option Mappers"){
    val some1: Option[Int] = Some(1)
    val none: Option[Int] = None
    assert( some1.toJson == JsNumber(1))
    assert( JsNumber(1).convertTo[Option[Int]] == Some(1))
    assert( none.toJson == JsNull)
    assert( JsNull.convertTo[Option[Int]] == None)
  }

  test("JsValue show test") {
    case class User2(name: String, age: Int, owner: User2 | Null = null) derives JsValueMapper

    val user = User2("John\t\r\n\"abc\"", 30, null)
    val child = User2("中国人", 10, user)

    println("child = " + child.toJson.show())

    assert(child.toJson.show().parseJson.convertTo[User2] == child)

  }

  test("Map[String,Int]") {
    val map = Map("a"->1,"b"->2,"c"->3)
    val js = map.toJson
    assert(js == json"""{"a":1,"b":2,"c":3}""")
    assert(js.convertTo[Map[String,Int]] == map)
  }

  test("Map[User, Int]") {
    case class User(name: String, age: Int) derives JsValueMapper
    val map = Map(User("a",1)->1,User("b",2)->2,User("c",3)->3)
    val js = map.toJson
    assert(js ==
      json"""[ { "key": { "name": "a", "age": 1 }, "value": 1 },
            { "key": { "name": "b", "age": 2 }, "value": 2 },
            { "key": { "name": "c", "age": 3 }, "value": 3 } ]""")
    assert(js.convertTo[Map[User,Int]] == map)
  }

  test("Map[User, User]") {
    case class User(name: String, age: Int) derives JsValueMapper
    val map = Map(User("a",1)->User("b",2),User("c",3)->User("d",4))
    val js = map.toJson
    assert(js ==
      json"""[ { "key": { "name": "a", "age": 1 }, "value": { "name": "b", "age": 2 } },
            { "key": { "name": "c", "age": 3 }, "value": { "name": "d", "age": 4 } } ]""")
    assert(js.convertTo[Map[User,User]] == map)
  }

}
