package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.{*, given}

class TestJson5Parser extends AnyFunSuite {

  test("parse json5") {
    val json5 = """{
                  |  // comments
                  |  unquoted: 'and you can quote me on that',
                  |  singleQuotes: 'I can use "double quotes" here',
                  |  lineBreaks: "Look, Mom! \
                  | No \\n's!",
                  |  hexadecimal: 0xdecaf,
                  |  leadingDecimalPoint: .8675309, andTrailing: 8675309.,
                  |  positiveSign: +1,
                  |  trailingComma: 'in objects', andIn: ['arrays',],
                  |  "backwardsCompatible": "with JSON",
                  |}""".stripMargin
    val input = ParserInput(json5)
    val json: JsValue.JsObject = Json5Parser(input).parseJsValue().asInstanceOf[JsValue.JsObject]
    assert(json.field("unquoted") == JsString("and you can quote me on that"))

    val expect = JsObject(
      "unquoted" -> "and you can quote me on that",
      "singleQuotes" -> "I can use \"double quotes\" here",
      "lineBreaks" -> "Look, Mom! \n No \\n's!",
      "hexadecimal" -> 912559,
      "leadingDecimalPoint" -> 0.8675309,
      "andTrailing" -> 8675309.0,
      "positiveSign" -> 1,
      "trailingComma" -> "in objects",
      "andIn" -> Array("arrays"),
      "backwardsCompatible" -> "with JSON"
    )

    assert(json == expect)
  }
}
