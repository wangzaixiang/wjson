package wjson

import org.scalatest.funsuite.AnyFunSuite

class TestPatterns extends AnyFunSuite {

  test("simple patterns") {
    val js = json"""{
      "a": 1,
      "b": "hello",
      "c": true
    }"""

    js match {
      case rejson"""{
        "a": ${a}@integer,
        "b": ${b}@string,
        "c": ${c}@boolean
      }""" =>
        println("a=$a, b=$b, c=$c")
    }

  }

}
