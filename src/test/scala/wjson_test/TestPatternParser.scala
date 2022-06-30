package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsPatternParser
import wjson.JsPattern.*

class TestPatternParser extends AnyFunSuite {

  test("simple program") {

    val program = """{name: "wangzx", age: 18}"""

    val result = JsPatternParser.parseRejson(program)

    assert(result  == Variable(null, ObjPattern(
            Map("name" -> Variable(null, StringPattern("wangzx")),
                "age" -> Variable(null, NumberPattern(18)))
      )))

  }

  test("simple program with binding") {

    val program = """{name: "wangzx", "age": age@18}"""

    val result = JsPatternParser.parseRejson(program)

    assert(result  == Variable(null, ObjPattern(
      Map("name" -> Variable(null, StringPattern("wangzx")),
        "age" -> Variable("age", NumberPattern(18)))
    )))

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
      Map("name1" -> Variable(null, StringPattern("wangzx")),
        "name2" -> Variable(null, StringPattern("wangzx")),
        "name3" -> Variable(null, StringPattern("wangzx")),
        "name4" -> Variable(null, NullPattern()),
        "field5" -> Variable(null, AnyVal(GroundType.NUMBER)),
        "field6" -> Variable(null, AnyVal(GroundType.STRING)),
        "field7" -> Variable(null, BoolPattern(true)),
        "field8" -> Variable(null, BoolPattern(false)),
        "field9" -> Variable(null, AnyVal(GroundType.INTEGER)),
        "more/age" -> Variable("age", NumberPattern(18)),
        "arr" -> Variable("arr", ArrPattern(
          List(
            Variable(null, NumberPattern(1)),
            Variable(null, NumberPattern(2)),
            Variable(null, NumberPattern(3)),
            Variable("x", AnyVals())
          ))),
        "arr2" -> Variable(null, ArrPattern(
          List(
            Variable("a", NumberPattern(1)),
            Variable("b", NumberPattern(2)),
            Variable("c", NumberPattern(3))
          ))),
        "obj" -> Variable("o", ObjPattern(
          Map("name" -> Variable(null, StringPattern("wangzx")),
            "age" -> Variable("age", NumberPattern(18)))
        )),
        (null:String) -> Variable("anys", AnyVals())))
    ))

  }



}
