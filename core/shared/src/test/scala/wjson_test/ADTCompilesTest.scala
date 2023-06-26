package wjson_test

import wjson.{*, given}

object ADTCompilesTest {
  object Works1:
    case class User3(name: String, age: Int, languages: List[List[User3]])derives JsValueMapper // OK

  object Works2:
    case class User3(name: String, age: Int, languages: List[User3])derives JsValueMapper // OK

  object Works3:
    case class User3(name: String, age: Int, languages: User3)derives JsValueMapper // Not OK

  object Works4:
    case class User3(name: String, age: Int, languages: User3)
    given JsValueMapper[User3] = JsValueMapper.derived[User3] // NOT OK

  object Works5 :
    case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
    val mapper: JsValueMapper[User3] = summon[JsValueMapper[User3]]

  object Works6 :
    case class User3(name: String, age: Int, languages: User3)derives JsValueMapper
    case class Family(mother: User3, father: User3, children: List[User3], relate: Family)derives JsValueMapper

  object Works7 :
    case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
    case class Family(mother: User3, father: User3, children: List[User3], relate: Family)
    def main(args: Array[String]): Unit =
      val a: JsValueMapper[Family] = JsValueMapper.derived[Family] // 4

  object Sum1:
    enum Color derives JsValueMapper:
      case Red, Green, Blue
      case RGB(red: Int, green: Int, blue: Int)
      case Alpha(red: Int, green: Int, blue: Int, alpha: Float)

  object Mixed:
    enum Color derives JsValueMapper:
      case Red, Green, Blue
      case RGB(red: Int, green: Int, blue: Int)
      case Alpha(red: Int, green: Int, blue: Int, alpha: Float)
      case Nothing(family: Family)

    case class User3(name: String, age: Int, languages: User3, color: Color) derives JsValueMapper
    case class Family(mother: User3, father: User3, children: List[User3], relate: Family, color: Color) derives JsValueMapper

  object TypeTest:
    type NAME = String
    type NMAES = List[NAME]
    type USERS = List[User3]
    case class User3(name: NAME, names: NMAES, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
    case class Family(mother: User3, father: User3, children: USERS, relate: Family)

    def main(args: Array[String]): Unit =
      val a: JsValueMapper[Family] = JsValueMapper.adtMapper[Family] // 4

  object OpaqueTypeTest:
    opaque type NAME = String
    opaque type NMAES = List[NAME]
    opaque type USERS = List[User3]

    case class User3(name: NAME, names: NMAES, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK

    case class Family(mother: User3, father: User3, children: USERS, relate: Family)

    def main(args: Array[String]): Unit =
      val a: JsValueMapper[Family] = JsValueMapper.adtMapper[Family] // 4
}
//

