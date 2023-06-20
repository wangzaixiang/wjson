package wjson_test

import org.scalatest.funsuite.AnyFunSuite

import wjson.{*, given}

class TestOrType extends AnyFunSuite {

  test("ortype") {

    case class Bean( name: String | Int | Null, other: Option[String]|Int) //8
    val bean = Bean("hello", 5)
    val js1 = bean.toJson

    val bean2 = js1.convertTo[Bean]
    assert(bean2 == bean)

  }

  test("simple ortype") {
    type StrOrInt = String | Int
    case class Bean(name: StrOrInt) //8
    val v1 = Bean("100")
    val v2 = Bean(50)

    val js1 = v1.toJson
    val js2 = v2.toJson

    val v11 = js1.convertTo[Bean]
    val v21 = js2.convertTo[Bean]

    assert(v11 == v1)
    assert(v21 == v2)
  }


}
