package wjson_test

import wjson.{JsValueMapper, JsValueMapperMacro}
import org.scalatest.funsuite.AnyFunSuite
import wjson.{given, *}

class TestCaseClass extends AnyFunSuite {

  // 1: given x: JsValueMapper[Address2] = JsValueMapperMacro.generate[Address2]
  // 2: case class Address2(state: String, city: String) derives JsValueMapper
  // 3: import wjson.CaseClassMappers.given

  test("using CaseValueMappers api") {
    // import wjson.CaseValueMappers.given
    case class Address2(state: String, city: String) derives JsValueMapper

    val address = Address2("guangdong", "guangzhou")
    val js = json"{state:'guangdong', city:'guangzhou'}"
    assert( address.toJson == js )
    assert( address.toJson == js )

    val address2 = js.to[Address2]
    assert( address2 == address)

  }

}
