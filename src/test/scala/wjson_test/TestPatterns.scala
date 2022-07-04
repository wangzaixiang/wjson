package wjson_test

import scala.language.implicitConversions

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsValue.{JsArray, JsNumber, JsObject}
import wjson.{*, given}

class TestPatterns extends AnyFunSuite {

  test("simple values") {
    json"1" match {
      case rejson"${i}@integer" => assert(i == 1)
      case _ => assert(false)
    }
  }

  test("simple patterns") {
    val js =
      json"""{
      "a": 1,
      "b": "123",
      "c": true,
      "d": "ddd",
      "e": null
    }"""

    js match {
      case rejson"""{
        "a": ${a}@integer,
        "b": ${b}@string,
        "c": ${c}@_,
        "d": ${d}@"ddd",
        "e": ${e}@null
        }""" =>
        // println(s"a=$a, b=$b, c=$c, d=$d")
        assert(a == 1)
        assert(b == "123")
        assert(c == JsBoolean(true) )
        assert(d == "ddd")
        assert(e == null)
      case _ => assert(false)
    }
  
    json"1" match {
      case rejson"""$a@1""" =>
        assert(a == 1)
      case _ => assert(false)
    }
    json"true" match {
      case rejson"""$a@_""" =>
        assert(a == JsBoolean(true))
      case _ => assert(false)
    }
    json"[1,2,3]" match {
      case rejson"""$a@_*""" =>
        assert(a == JsArray(JsNumber(1), JsNumber(2), JsNumber(3)))
      case _ => assert(false)
    }
    json"{a: 1}" match {
      case rejson"""$a@{a:1}""" =>
        assert(a == JsObject("a" -> JsNumber(1)))
      case _ => assert(false)
  }

  }

  
  test("type matching") {
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

  test("value matching") {
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

  test("array matching") {
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

  test("nested object matching") {
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
          obj/biz/list: ${list}@[_,${snd}@integer,3],
          obj: $obj@_,
          obj2/foo2: ${foo2}@{bar2: 123, baz2: "abc"},
          obj3: {
            objArr: $array@[$f@{a:$aaa@1,b:$bb@integer}, $rest3@_*],
          },
        }""" =>
        assert(foo == JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")))
        assert(other == JsObject(Map("bar" -> JsNumber(123))))
        assert(baz == "abc")
        assert(obj == JsObject("foo" -> JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")), "biz" -> JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))))
        assert(foo2 == JsObject("bar2" -> JsNumber(123), "baz2" -> JsString("abc")))
        assert(biz == JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5))))
        assert(list == JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))
        assert(first == 1)
        assert(snd == 2)
        assert(rest == JsArray(List(JsNumber(4), JsNumber(5))))
        assert(array == JsArray(JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)), JsObject("a" -> JsNumber(3), "b" -> JsNumber(4))))
        assert(rest3 == JsArray(List(JsObject("a" -> JsNumber(3), "b" -> JsNumber(4)))))
        assert(f == JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)))
        assert(aaa == 1)
        assert(bb == 2)
      case _ => assert(false)
    }
  }

  test("anyVals in object") {
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
        assert(other == JsObject(Map("c" -> JsBoolean(true))))
      case _ => assert(false)
    }
  }

  test("anyVals in array") {
    val js =
      json"""{
      "a": [1,2,3,4,5],
    }"""

    js match {
      case rejson"""
         {
          a: ${a}@[1,2,3, ${other}@_*, 5],
        }""" =>
        assert(a == JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))
        assert(other == JsArray(JsNumber(4), JsNumber(5)))
      case _ => assert(false)
    }
    js match {
      case rejson"""
         {
          a: ${a}@[1,2,${other}@_*],
        }""" =>
        assert(a == JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))
        assert(other == JsArray(JsNumber(3), JsNumber(4), JsNumber(5)))
      case _ => assert(false)
    }
  }
  
  test("unapply to map") {
    val js =
      json"""{
      "a": 1,
      "obj": {
          "foo": {
            "bar": [1,2,3], baz: "abc", far: true
           },
           objArr: [{a: 1, b: 2}, {a: 3, b: 4}]
      },
      b:234
      
    }"""
    
    new RejsonInterpolation(
      StringContext.apply(
        """
          {
            a: a@_,
            obj/objArr: [{a:1,b:2}, _*],
            obj: obj@{foo: foo@_, objArr: o@[_, aa@_*]},
            other@_*,
          }""")).unapplyAsMap(js) match
      case Some(map) =>
        assert(map("a") == JsNumber(1))
        assert(map("obj") == JsObject("foo" -> JsObject("bar" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3)), "baz" -> JsString("abc"), "far" -> JsBoolean(true)), "objArr" -> JsArray(JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)), JsObject("a" -> JsNumber(3), "b" -> JsNumber(4)))))
        assert(map("other") == JsObject("b" -> JsNumber(234)))
      case _ => assert(false)
  }
}
