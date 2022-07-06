package wjson

object Main {

  def main(args: Array[String]): Unit = {
      val json = """{name: 'wangzx', age: 20, email:'www@qq.com'}"""
      val pattern = "{name: n@_, other @ _*}"
      rejsonMatch(json, pattern) match
        case Some(result) => println("matched:" + result)
        case null => println("not matched")
  }

  def rejsonMatch(json: String, pattern: String): Option[Map[String, Any]] =
    val jsv = new JsonParser(ParserInput(json), true).parseJsValue()
    val jsp = JsPatternParser.parseRejson(pattern)

    RejsonMatcher(jsp).unapplyAsMap(jsv)

}
