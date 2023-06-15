package demo

import wjson.JsValueMapper

//
//object ItWorks {
//  enum Color derives JsValueMapper:
//    case Red, Green, Blue
//
//  def main(args: Array[String]): Unit = {
////    given JsValueMapper[Color] = JsValueMapper.derived[Color]
//  }
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
