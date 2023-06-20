package demo

import wjson.{*, given}

//object ItWorks {
//  enum Color derives JsValueMapper:
//    case Red, Green, Blue
//}
//
//class NotWorks {
//  enum Color2 derives JsValueMapper:
//    case Red, Green, Blue
//
//  given JsValueMapper[Color2] = JsValueMapper.derived[Color2]
//}
//
//object NotWorks2 {
//
//  def main(args: Array[String]): Unit = {
//    enum Color3 derives JsValueMapper:
//      case Red, Green, Blue
//  }
//}

object Mixed:
  enum Color derives JsValueMapper:
    case Red, Green, Blue
    case RGB(red: Int, green: Int, blue: Int)
    case Alpha(red: Int, green: Int, blue: Int, alpha: Float)
    case Nothing(family: Family)

  case class User3(name: String, age: Int, languages: User3, color: Color)derives JsValueMapper

  case class Family(mother: User3, father: User3, children: List[User3], relate: Family, color: Color)derives JsValueMapper


