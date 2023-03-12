package wjson_test

import wjson.*
import org.scalatest.funsuite.AnyFunSuite

case class User(name: String, friends: List[User])  // derives JsValueMapper

class TestSelfReference  extends AnyFunSuite {

    test("self reference"){
        given JsValueMapper[User] = JsValueMapperMacro.genADT[User]
        val user1 = User("user1", Nil)
        val user2 = User("user2", List(user1));

        println(user2.toJson)

    }

}
