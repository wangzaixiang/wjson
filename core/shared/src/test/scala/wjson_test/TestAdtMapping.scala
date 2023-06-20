package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.{*, given}

object TestAdtMapping:
  enum WeekDay2:
    case Mon, Tue, Wed, Thu, Fri, Sat, Sun

  enum Color:
    case Red, Green, Blue
    case Custom(r: Int, g: Int, b: Int)
    case Alpha(r: Int, g: Int, b: Int, a: Float)


class TestAdtMapping extends AnyFunSuite {

  test("simple product") {
    case class User1(name: String, age: Int) derives JsValueMapper
    case class User2(name: String, age: Int)

    given JsValueMapper[User2] = JsValueMapper.derived[User2]
  }

  test("circle reference") {
    case class User(name: String, age: Int) // derives JsValueMapper
    case class Family(mother: User, father: User, children: List[List[User]], relate: Family|Null = null)

    val f1 = Family(User("month", 40), User("father", 42),
      Nil, null)
    val family: Family = Family(User("Julie", 40),
      User("John", 42),
      List(List(User("Chris", 13), User("Meg", 16))),
      f1
    )
    val js = family.toJson // 4
    val family2 = js.convertTo[Family]

    assert(family2 == family)
  }

//  test("test widen"){
//    case class User1(name: String, age: Int, languages: List[String]) derives JsValueMapper
//    case class User2(name: String, age: Int, languages: collection.immutable.List[String]) derives JsValueMapper
//    case class User3(name: String, age: Int, languages: List[List[String]])
//
//    case class Family(mother: User3, father: User3, children: List[List[User3]], relate: Family = null) derives JsValueMapper
//  }

  test("simple enum") {
    enum WeekDay  derives JsValueMapper  {
      case Mon, Tue, Wed, Thu, Fri, Sat, Sun
    }
    val days = WeekDay.values.toList
    val js = days.toJson
  }

  test("enums is only supported in global scope") {
    import TestAdtMapping.*

    val color = Color.Alpha(1, 2, 3, 0.5f)
    val js = color.toJson
    println(js.showPretty)
  }

}
