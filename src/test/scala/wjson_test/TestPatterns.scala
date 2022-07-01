package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsValue.{JsArray, JsNumber}
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
      "a": {"b": {c: "ccc", d: 123}}
    }"""
    
    js match {
      case rejson"""
         {
          a: ${a}@{
            b: {
              c: ${c}@"ccc",
              d: ${d}@123,
            }
          }
        }""" =>
        assert(a == JsObject("b" -> JsObject("c" -> JsString("ccc"), "d" -> JsNumber(123))))
        assert(c == "ccc")
        assert(d == 123)
      case _ => assert(false)
    }
    
//    js match {
//      case rejson"""
//         {
//          a/b/c: ${c}@"ccc",
//          a/b/d: ${d}@123,
//        }""" =>
//        assert(c == "ccc")
//        assert(d == 123)
//      case _ => assert(false)
//    }
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
