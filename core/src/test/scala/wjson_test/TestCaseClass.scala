package wjson_test

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite
import wjson.{*, given}

class TestCaseClass extends AnyFunSuite {

  // 1: given x: JsValueMapper[Address2] = JsValueMapperMacro.generate[Address2]
  // 2: case class Address2(state: String, city: String) derives JsValueMapper
  // 3: import wjson.CaseClassMappers.given

  test("using derives") {
    case class Address1(state: String, city: String) derives JsValueMapper

    val address = Address1("guangdong", "guangzhou")
    val js = json"""{"state":"guangdong", "city":"guangzhou"}"""
    assert( address.toJson == js )   // dont generate anonymous mapper here
    assert( address.toJson == js )   // dont generate anonymous mapper here

    val address2 = js.convertTo[Address1]   // dont generate anonymous mapper here
    assert( address2 == address)
  }

  test("option field"){
    case class User(name: String, age: Int, email: Option[String]) derives JsValueMapper
    val user = User("John", 20, None)
    val jso =  json"""{"name":"John", "age":20 }"""
    assert( user.toJson == jso)
    assert( jso.convertTo[User] == user)

    val user2 = User("John", 20, Some("John@qq.com"))
    val jso2 = json"""{"name":"John", "age":20, "email":"John@qq.com"}"""
    assert( user2.toJson == jso2)
    assert( jso2.convertTo[User] == user2)
  }

  test("default field") {
    case class User2(name: String, age: Int = 18, email: Option[String] = Some("xx@qq.com")) derives JsValueMapper
    val user = User2("John", 20)
    val jso =  json"""{"name":"John", "age":20, "email": "xx@qq.com" }"""
    assert( user.toJson == jso)
    assert( jso.convertTo[User2] == user)

    assert( json"""{"name":"John"}""".convertTo[User2] == User2("John", 18, Some("xx@qq.com")) )
    assert( User2("John", email=None).toJson == json"""{"name":"John", "age":18}""")
  }

  test("raw JsValue fields") {
    case class User3(name: String, age:Int, address: JsObject) derives JsValueMapper
    val user = User3("John", 20, JsObject("city"->"guangzhou", "state"->"gd") )

    assert(user.toJson == json"""{"name":"John", "age":20, "address":{"city":"guangzhou", "state":"gd" } }""")

    assert( user.toJson.convertTo[User3] == user)
  }

  test("Map[String, JsValue] fields") {
    case class User3(name: String, age:Int, address: JsObject, features: Map[String, JsValue])  derives JsValueMapper

    val jsonStr = json"""{"name":"John", "age":20, "address":{"state": "gd", "city":"guangzhou"}, "features":{"love":"read a book"}}"""
    assert( jsonStr.convertTo[User3] ==
      User3("John", 20, json"""{"state":"gd", "city":"guangzhou"}""".asInstanceOf[JsObject], Map("love"->"read a book")) )
  }

  test("collections") {
    case class User4(name: String, likes: List[String], scores: Map[String, Int])

    val user4 = User4("John", List("java", "scala"), Map("java"->100, "scala"->200))
    assert( user4.toJson == json"""{"name":"John", "likes": ["java", "scala"], "scores":{ "java":100, "scala":200} }""")



    assert( user4.toJson.convertTo[User4] == user4)
  }

  test("explict given value") {
    case class Address2(state: String, city: String)

    given mapper: JsValueMapper[Address2] = JsValueMapper.derived[Address2]

    val address = Address2("guangdong", "guangzhou")
    val js = json"""{"state":"guangdong", "city":"guangzhou"}"""
    assert( address.toJson == js )  // dont generate anonymous mapper here
    assert( address.toJson == js )  // dont generate anonymous mapper here

    val address2 = js.convertTo[Address2]  // dont generate anonymous mapper here
    assert( address2 == address)
  }

  test("import CaseClassMappers.given") {
    case class Address3(state: String, city: String)

    val address = Address3("guangdong", "guangzhou")
    val js = json"""{"state":"guangdong", "city":"guangzhou"}"""
    assert( address.toJson == js )  // generate anonymous mapper here 1
    assert( address.toJson == js )  // generate anonymous mapper here 2

    val address2 = js.convertTo[Address3]  // generate anonymous mapper here 3
    assert( address2 == address)
  }

  test("case class mappers"){

    case class User1(name: String, age: Int, address: Address1) derives JsValueMapper
    case class Address1(street: String, city: String) derives JsValueMapper

    val user = User1("John", 30, Address1("Main St", "New York"))
    assert( user.toJson == JsObject("name"->"John","age"->30,
      "address"->JsObject("street"->"Main St","city"->"New York")))
    assert( json"""{"name":"John","age":30,"address":{"street":"Main St","city":"New York"}}""".convertTo[User1] == user)

  }

  test("Self Reference Case Class") {
    case class User2(name: String, age: Int, owner: User2|Null = null) derives JsValueMapper

    val user = User2("John", 30, null)
    val child = User2("Mary", 10, user)

    val js = child.toJson

    assert( js == JsObject("name"->"Mary","age"->10,
      "owner"->JsObject("name"->"John","age"->30)))
    assert( js.convertTo[User2] == child)

  }

}
