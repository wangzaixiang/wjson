package wjson_test

import wjson.{JsValueMapper, JsValueMapperMacro}
import org.scalatest.funsuite.AnyFunSuite
import wjson.{given, *}

class TestCaseClass extends AnyFunSuite {

  // 1: given x: JsValueMapper[Address2] = JsValueMapperMacro.generate[Address2]
  // 2: case class Address2(state: String, city: String) derives JsValueMapper
  // 3: import wjson.CaseClassMappers.given

  test("using derives") {
    case class Address1(state: String, city: String) derives JsValueMapper

    val address = Address1("guangdong", "guangzhou")
    val js = json"{state:'guangdong', city:'guangzhou'}"
    assert( address.toJson == js )   // dont generate anonymous mapper here
    assert( address.toJson == js )   // dont generate anonymous mapper here

    val address2 = js.to[Address1]   // dont generate anonymous mapper here
    assert( address2 == address)
  }

  test("explict given value") {
    case class Address2(state: String, city: String)

    given mapper: JsValueMapper[Address2] = JsValueMapperMacro.generate[Address2]

    val address = Address2("guangdong", "guangzhou")
    val js = json"{state:'guangdong', city:'guangzhou'}"
    assert( address.toJson == js )  // dont generate anonymous mapper here
    assert( address.toJson == js )  // dont generate anonymous mapper here

    val address2 = js.to[Address2]  // dont generate anonymous mapper here
    assert( address2 == address)
  }

  test("import CaseClassMappers.given") {
    case class Address3(state: String, city: String)

    import wjson.CaseClassMappers.given

    val address = Address3("guangdong", "guangzhou")
    val js = json"{state:'guangdong', city:'guangzhou'}"
    assert( address.toJson == js )  // generate anonymous mapper here 1
    assert( address.toJson == js )  // generate anonymous mapper here 2

    val address2 = js.to[Address3]  // generate anonymous mapper here 3
    assert( address2 == address)
  }

}
