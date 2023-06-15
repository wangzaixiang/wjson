package wjson_test

import org.scalatest.funsuite.AnyFunSuite

import wjson.{*, given}

class TestOrType extends AnyFunSuite {

//  test("ortype") {
//
//    case class Bean( name: String | Int)
//
//    val bean = Bean("hello")
//    bean.toJson
//
//  }

  test("example1") {
    case class User(name: String, age: Int)
    case class Family(mother: User, father: User, children: List[User])

    val family = Family(User("Julie", 40), User("John", 42), List(User("Chris", 13), User("Meg", 16)))
    val js = family.toJson // 3

    println(js)
  }

}
