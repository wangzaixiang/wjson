package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.*

class TestTagStringPatterns extends AnyFunSuite {

  test("mvel expressions"){

    json"""{
          str: "123456", num: 10,
          addr: { state: 'gd', city: 'gz' },
          scores: [ 1,2,3,4,5 ]
    }""" match
      case rejson"""{
        num: ${a}@mvel'it % 2 == 0',
        str: ${b}@mvel'it.startsWith("12")',
        addr: mvel'it.state == "gd"',
        scores: mvel'it[2] == 3'
       } """ =>
        assert(a == JsNumber(10))
        assert(b == JsString("123456"))
        true
      case _ => assert(false, "not matched")

  }

  // { a: { b: [ {c:}, {c:} ] }
  // a.b[pattern].c => [ c1, c2 ]

}
