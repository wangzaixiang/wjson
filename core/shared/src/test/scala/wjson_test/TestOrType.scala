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


}
