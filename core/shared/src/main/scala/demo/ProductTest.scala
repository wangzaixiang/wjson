package demo

import wjson.{*, given}

// 在 derived generate code 中，如果出现了递归，则报错
object Works1 {
  case class User3(name: String, age: Int, languages: List[List[User3]]) derives JsValueMapper // OK
}

object Works2 {
  case class User3(name: String, age: Int, languages: List[User3]) derives JsValueMapper // OK
}

object NotWorks3 {
  case class User3(name: String, age: Int, languages: User3)derives JsValueMapper // Not OK
}

object NotWorks4 {
  case class User3(name: String, age: Int, languages: User3);
  given JsValueMapper[User3] = JsValueMapper.derived[User3] // NOT OK
}

object Works5 {
  case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
  val mapper = summon[JsValueMapper[User3]]
}



object NotWorks6 {
  case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
  case class Family(mother: User3, father: User3, children: List[User3], relate: Family)  derives JsValueMapper
  object Family:
    given a: JsValueMapper[Family] = JsValueMapper.derived[Family] // 4
  end Family
}

object Works7 {
  case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK
  case class Family(mother: User3, father: User3, children: List[User3], relate: Family)

  def main(args: Array[String]): Unit = {
    val a: JsValueMapper[Family] = JsValueMapper.derived[Family] // 4
  }
}

