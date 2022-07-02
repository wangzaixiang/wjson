package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.{given, *}

class TestPatterns extends AnyFunSuite {

  test("simple patterns") {
    val js =
      json"""{
      "a": 1,
      "b": "123",
      "c": true,
      "d": "ddd"
    }"""

    js match {
      case rejson"""{
        "a": ${a}@integer,
        "b": ${b}@string,
        "c": ${c}@_,
        "d": ${d}@"ddd"
        }""" =>
        println(s"a=$a, b=$b, c=$c, d=$d")
        assert(a == 1)
        assert(b == "123")
        assert(c.asInstanceOf[JsBoolean].value == true)
        assert(d == "ddd")
    }

  }


  test("test type matching") {
    val js =
      json"""{
      "a": 1,
      "b": "hello",
      "c": true,
      "d": 12.3
    }"""

    js match {
      case rejson"""
         {
          "a": integer,
          "b": string,
          "c": boolean,
          "d": number
        }""" =>
      case _ => assert(false)
    }

    js match {
      case rejson"""
         {
          "a": ${a}@integer,
          "b": ${b}@string,
          "c": ${c}@boolean,
          "d": ${d}@number
        }""" =>
        assert(a == 1)
        assert(b == "hello")
        assert(c == true)
        assert(d == 12.3)
      case _ => assert(true)
    }

  }

  test("test value matching") {
    val js =
      json"""{
      "a": 1,
      "b": "hello",
      "c": true,
      "d": 12.3,
      "e": -12
    }"""

    js match {
      case rejson"""
         {
          "a": 1,
          "b": "hello",
          "c": true,
          "d": 12.3,
          "e": -12
        }""" => assert(true)
      case _ => assert(false)
    }

    js match {
      case rejson"""
         {
          "a": ${a}@1,
          "b": ${b}@"hello",
          "c": ${c}@true,
          "d": ${d}@12.3,
          "e": ${e}@-12
        }""" =>
        assert(a == 1)
        assert(b == "hello")
        assert(c == true)
        assert(d == 12.3)
        assert(e == -12)
      case _ => assert(false)
    }

  }

  test("test array matching") {
    val js =
      json"""{
      "a": [1,2,3],
    }"""

    js match {
      case rejson"""
         {
          "a": $arr@[1,2,3],
        }""" =>
        assert(arr == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
      case _ =>
        assert(false, "should not match")
    }

    js match {
      case rejson"""
         {
          "a": $arr@[1,2,4],
        }""" =>
        assert(arr == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
      case _ =>
        assert(true, "should not match")
    }

    js match {
      case rejson"""
         {
          "a": ${arr}@[${a1}@1,${a2}@integer,3],
        }""" =>
        assert(arr == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
        assert(a1 == 1)
        assert(a2 == 2)
      case _ => assert(false)
    }
    js match {
      case rejson"""
         {
          "a": ${arr}@_,
        }""" =>
        assert(arr.asInstanceOf[JsArray].elements.size == 3)
      case _ => assert(false)
    }
  }

  test("test nesting object matching") {
    val js =
      json"""{
      "obj": {"foo": {"bar": 123, baz: "abc", far: true}, biz: {list: [1,2,3,4,5]}},
      "obj2": {"foo2": {"bar2": 123, baz2: "abc"}},
      "obj3": {objArr: [{a: 1, b: 2}, {a: 3, b: 4}]},
    }"""

    js match {
      case rejson"""
        {
          obj/foo: ${foo}@{far:boolean, ${other}@_*},
          obj/foo/baz: ${baz}@"abc",
          obj/biz: ${biz}@{list:[$first@1, ${rest}@_*], _*},
          obj/biz/list: ${list}@[1,${snd}@integer,3],
          obj: $obj@_,
          obj2/foo2: ${foo2}@{bar2: 123, baz2: "abc"},
          obj3: {
            objArr: $array@[$f@{a:$aaa@1,b:$bb@integer}, $rest3@_*],
          },
        }""" =>
        assert(foo == JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")))
        assert(other == Map("bar" -> JsNumber(123)))
        assert(baz == "abc")
        assert(obj == JsObject("foo" -> JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")), "biz" -> JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))))
        assert(foo2 == JsObject("bar2" -> JsNumber(123), "baz2" -> JsString("abc")))
        assert(biz == JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5))))
        assert(list == JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))
        assert(first == 1)
        assert(snd == 2)
        assert(rest == List(JsNumber(4), JsNumber(5)))
        assert(array == JsArray(JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)), JsObject("a" -> JsNumber(3), "b" -> JsNumber(4))))
        assert(rest3 == List(JsObject("a" -> JsNumber(3), "b" -> JsNumber(4))))
        assert(f == JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)))
        assert(aaa == 1)
        assert(bb == 2)
      case _ => assert(false)
    }
  }

  test("test anyVals in object") {
    val js =
      json"""{
      "a": "1",
      "b": 2,
      "c": true,
    }"""

    js match {
      case rejson"""
         {
          a: ${a}@"1",
          ${other}@_*,
          b: ${b}@2,
        }""" =>
        assert(a == "1")
        assert(b == 2)
        assert(other == Map("c" -> JsBoolean(true)))
      case _ => assert(false)
    }
  }

  test("test anyVals in array") {
    val js =
      json"""{
      "a": [1,2,3]
    }"""

    js match {
      case rejson"""
         {
          a: ${a}@[1,2,3, ${other}@_*],
        }""" =>
        assert(a == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
        assert(other == List())
      case _ => assert(false)
    }
    js match {
      case rejson"""
         {
          a: ${a}@[1,2,${other}@_*],
        }""" =>
        assert(a == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
        assert(other == List(JsNumber(3)))
      case _ => assert(false)
    }
  }
}
