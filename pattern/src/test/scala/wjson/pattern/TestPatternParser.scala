package wjson.pattern

import org.scalatest.funsuite.AnyFunSuite
import wjson.pattern.*
import wjson.pattern.JsPattern.*

class TestPatternParser extends AnyFunSuite {

  test("simple program") {

    val program = """{name: "wangzx", age: 18}"""

    val result = JsPatternParser.parseRejson(program)

    assert(result  == Variable(null, ObjPattern(
            "name" -> Variable(null, StringPattern("wangzx")),
            "age" -> Variable(null, NumberPattern(18L)))
    ))

  }


  test("big Long") {

    NumberPattern(589458537707843585L) match
      case NumberPattern(value: Long) =>
        assert(value == 589458537707843585L)
      case _ => assert(false)

  }


  test("simple program with binding") {

    val program = """{name: "wangzx", "age": age@18}"""

    val result = JsPatternParser.parseRejson(program)

    assert(result  == Variable(null, ObjPattern(
      "name" -> Variable(null, StringPattern("wangzx")),
      "age" -> Variable("age", NumberPattern(18L)))
    ))

  }

  test("a complete program") {

    val program =
    """
    {
      name1: "wangzx",
      "name2": "wangzx",
      'name3': 'wangzx',
      name4: null,
      field5: number,
      field6: string, # comment
      field7: true,
      field8: false,
      field9: integer,
      more/age: age@18,
      arr: arr@[1, 2, 3, x@_*],
      arr2: [a@1, b@2, c@3],
      obj: o@{
        name: "wangzx",
        age: age@18,
      },
      anys@_*
    }"""

    val result = JsPatternParser.parseRejson(program)

    assert(result  == Variable(null, ObjPattern(
        "name1" -> Variable(null, StringPattern("wangzx")),
        "name2" -> Variable(null, StringPattern("wangzx")),
        "name3" -> Variable(null, StringPattern("wangzx")),
        "name4" -> Variable(null, NullPattern()),
        "field5" -> Variable(null, AnyVal(GroundType.NUMBER)),
        "field6" -> Variable(null, AnyVal(GroundType.STRING)),
        "field7" -> Variable(null, BoolPattern(true)),
        "field8" -> Variable(null, BoolPattern(false)),
        "field9" -> Variable(null, AnyVal(GroundType.INTEGER)),
        "more/age" -> Variable("age", NumberPattern(18L)),
        "arr" -> Variable("arr", ArrPattern(
          List(
            Variable(null, NumberPattern(1L)),
            Variable(null, NumberPattern(2L)),
            Variable(null, NumberPattern(3L)),
            Variable("x", AnyVals())
          ))),
        "arr2" -> Variable(null, ArrPattern(
          List(
            Variable("a", NumberPattern(1L)),
            Variable("b", NumberPattern(2L)),
            Variable("c", NumberPattern(3L))
          ))),
        "obj" -> Variable("o", ObjPattern(
          "name" -> Variable(null, StringPattern("wangzx")),
          "age" -> Variable("age", NumberPattern(18L)))
        ),
        (null: String) -> Variable("anys", AnyVals())
      ) ))

  }



}
