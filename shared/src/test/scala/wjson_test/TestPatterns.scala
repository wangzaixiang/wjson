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

  test("numbers"){
    json"10" match
      case JsNumber(x: Double) => assert(false)
      case JsNumber(x: Long) => assert(true)
      case _ => assert(false)

    json"10.0" match
      case JsNumber(x: Long) => assert(false)
      case JsNumber(x: Double) => assert(true)
      case _ => assert(false)
  }

  test("simple patterns") {
    val js =
      json"""{
      "a": 1,
      "b": "123",
      "b1": 100,
      "b2": 100.0,
      "c": true,
      "d": "ddd",
      "e": null,
      "f": [1.2],
      "g": {a: 1}
    }"""

    js match {
      case rejson"""{
        "a": ${a}@integer,
        "b": ${b}@string,
        "b1": ${b1}@integer,
        "b2": ${b2}@number,
        "c": ${c}@boolean,
        "d": ${d}@"ddd",
        "e": ${e}@null,
        "f": ${f}@array,
        "g": ${g}@object
        }""" =>
        // println(s"a=$a, b=$b, c=$c, d=$d")
        assert(a == 1)
        assert(b == "123")
        assert(b1 == new java.lang.Long(100L))
        assert(b2 == new java.lang.Double("100.0"))
        assert(c == true )
        assert(d == "ddd")
        assert(e == null)
        assert(f == List(JsNumber(1.2)))
        assert(g == Map("a" -> JsNumber(1)))
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
    import wjson.given
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
          obj/biz: ${biz}@{list:[$first@1 ], _*},
          obj/biz/list: ${list}@[_,${snd}@integer,3, 4, 5],
          obj: $obj@_,
          obj2/foo2: ${foo2}@{bar2: 123, baz2: "abc"},
          obj3: {
            objArr: $array@[$f@{a:$aaa@1,b:$bb@integer}, $rest3@_*],
          },
        }""" =>
        assert(foo == JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")))
        assert(other == JsObject("bar" -> 123, "baz" -> "abc"))
        assert(baz == "abc")
        assert(obj == JsObject("foo" -> JsObject("bar" -> JsNumber(123), "far" -> true, "baz" -> JsString("abc")), "biz" -> JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))))
        assert(foo2 == JsObject("bar2" -> JsNumber(123), "baz2" -> JsString("abc")))
        assert(biz == JsObject("list" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5))))
        assert(list == JsArray(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(4), JsNumber(5)))
        assert(first == 1)
        assert(snd == 2)
//        assert(rest == JsArray(List(JsNumber(4), JsNumber(5))))
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
        assert(other == JsArray(JsNumber(4)))
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

    new RejsonMatcher( """
          {
            a: a@_,
            obj/objArr: [{a:1,b:2}, _*],
            obj: obj@{foo: foo@_, objArr: o@[_, _*]},
            other@_*,
          }""").unapplyAsMap(js) match
      case Some(map) =>
        assert(map("a") == JsNumber(1))
        assert(map("obj") == JsObject("foo" -> JsObject("bar" -> JsArray(JsNumber(1), JsNumber(2), JsNumber(3)), "baz" -> JsString("abc"), "far" -> JsBoolean(true)), "objArr" -> JsArray(JsObject("a" -> JsNumber(1), "b" -> JsNumber(2)), JsObject("a" -> JsNumber(3), "b" -> JsNumber(4)))))
        assert(map("other") == JsObject("b" -> JsNumber(234)))
      case _ => assert(false)
  }

  test("array filters"){
    val js = json"""
      { users: [
          { name: 'John', age: 10, sex: 'male' },
          { name: 'Rose', age: 21, sex: 'female' },
          { name: 'Rose', age: 11, sex: 'female' },
          { name: 'steven', age:12, sex: 'male' }
        ]
      }
    """
    js match
      case rejson"""
           { users[{sex:'male'}] : ${u: JsValue}@_,
           }
           """ =>
        assert(u == json"[{name:'John', age:10, sex:'male'},{name:'steven', age:12, sex:'male'}]")
      case _ => assert(false)

    js match
      case rejson"""{users[{name:'Rose'}]: [ ${u1}@_, ${u2}@_ ] }""" =>
        assert(u1 == json"{age:21, name:'Rose', sex: 'female'}")
        assert(u2 == json"{age:11, name:'Rose', sex: 'female'}")
      case _ => assert(false)

    js match
      case rejson"""{users[{name:'Rose'}][0]:  ${u1}@_ }""" =>
        assert(u1 == json"{age:21, name:'Rose', sex: 'female'}")
      case _ => assert(false)

    js match
      case rejson""" { users[3]/name: 'steven' }  """ => assert(true)
      case _ => assert(false)

    js match
      case rejson""" { users/*/name: ['John', 'Rose', 'Rose', 'steven'] } """ =>
        assert(true)
      case _ => assert(false)
  }

  test("github commits") {
    val info =
      """
      {
    "sha": "650e56cd380c311909cd50408bbb4884f1f5d21e",
    "node_id": "C_kwDOHj94ltoAKDY1MGU1NmNkMzgwYzMxMTkwOWNkNTA0MDhiYmI0ODg0ZjFmNWQyMWU",
    "commit": {
      "author": {
        "name": "wangzaixiang",
        "email": "949631531@qq.com",
        "date": "2022-07-05T14:23:09Z"
      },
      "committer": {
        "name": "wangzaixiang",
        "email": "949631531@qq.com",
        "date": "2022-07-05T14:23:09Z"
      },
      "message": "add taged string pattern support\nadd array index/filter support",
      "tree": {
        "sha": "89fbbf82e3c5ffd0e1a7978dd7778195a004df2c",
        "url": "https://api.github.com/repos/wangzaixiang/wjson/git/trees/89fbbf82e3c5ffd0e1a7978dd7778195a004df2c"
      },
      "url": "https://api.github.com/repos/wangzaixiang/wjson/git/commits/650e56cd380c311909cd50408bbb4884f1f5d21e",
      "comment_count": 0,
      "verification": {
        "verified": false,
        "reason": "unsigned",
        "signature": null,
        "payload": null
      }
    },
    "url": "https://api.github.com/repos/wangzaixiang/wjson/commits/650e56cd380c311909cd50408bbb4884f1f5d21e",
    "html_url": "https://github.com/wangzaixiang/wjson/commit/650e56cd380c311909cd50408bbb4884f1f5d21e",
    "comments_url": "https://api.github.com/repos/wangzaixiang/wjson/commits/650e56cd380c311909cd50408bbb4884f1f5d21e/comments",
    "parents": [
      {
        "sha": "d90609ac4e7254eac5138453e2e07591a11bb55e",
        "url": "https://api.github.com/repos/wangzaixiang/wjson/commits/d90609ac4e7254eac5138453e2e07591a11bb55e",
        "html_url": "https://github.com/wangzaixiang/wjson/commit/d90609ac4e7254eac5138453e2e07591a11bb55e"
      }
    ]
}
      """

    info.parseJson() match
      case rejson"""
        {
          sha: $sha@_,
          commit: { author: { name: $commit_name@_ } },
          url: $url@_,
          parents/*/sha: $parents@_
        }
      """ =>
        println(s"sha = $sha, commit_name = $commit_name, url = $url, parents=$parents")
        assert(true)

  }


}
