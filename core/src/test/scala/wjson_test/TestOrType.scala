package wjson_test

import org.scalatest.funsuite.AnyFunSuite

import wjson.{*, given}

class TestOrType extends AnyFunSuite {

  test("ortype") {

    case class User(name: String)

    type T1 = String | Int | User | Null
    type T2 = Option[User] | String | Int
    case class Bean( name: T1, other: T2) derives JsValueMapper; //8

    val names: List[ T1 ] = List("hello", 5, User("wang"), null)
    //  val others: List[ T2 ] = List(Some("hello"), Some("world"), User("wang"), None, 5)
    val others: List[ T2 ] = List("hello", "world", Some(User("wang")), None, 5)

    for(name <- names; other <- others) {
        val bean = Bean(name, other)
        val js = bean.toJson
        println(js.show)
        val bean2 = js.convertTo[Bean]
        assert(bean2 == bean)
    }

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


  test("More Beans") {
      case class Bean1(name: String, age: Int)
      case class Bean2(name: String, age: Int)

      case class Root(bean: Bean1 | Bean2) derives JsValueMapper

      val root: Root = Root( Bean1("wang", 18) )
      val js = root.toJson
      println(js.show)
      assert( root == js.convertTo[Root] )

  }


}
