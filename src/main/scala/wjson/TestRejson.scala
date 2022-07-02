package wjson

/**
 * Created by WahsonLeung on 2022/6/28
 */
object TestRejson {
  
  def main(args: Array[String]): Unit = {
    val js =
      json"""{
      "a": 1,
      "obj": {"foo": {"bar": [1,2,3], baz: "abc", far: true}, objArr: [{a: 1, b: 2}, {a: 3, b: 4}]},
      "arr": [1,2,3,4],
      
    }"""
    
    js match {
      case rejson"""
      {
        obj: {
          objArr: [$f@{a:$aa@1,b:2},_*],
          _*
        },
        ${other}@_*,
        }""" =>
//        println(s"$bar")
//        println(s"$first, $snd")
//        println(s"$array")
        println(s"$f $aa")
//        println(s"$rest")
      //        println(s"a=$a, b=$b, c=$c, name=${uname}, age=$age, num=$num ")
      //        println(s"${a.asInstanceOf[Int]},${b.asInstanceOf[String]},${c.asInstanceOf[Boolean]}")
    }
    
    //    val js = JsObject(
    //      "name" -> JsString("John"),
    //      "age" -> JsNumber(30),
    //      "address" -> JsObject(
    //        "street" -> JsString("Main St"),
    //        "city" -> JsString("New York"),
    //        "state" -> JsString("NY")
    //      )
    //    )
    //    js match {
    //      case json"""{name: $name, age:$age, address: { state: "NY", city: $city } }""" =>
    //        assert( name == JsString("John") )
    //        assert( age == JsNumber(30) )
    //        assert( city == JsString("New York") )
    //    }
  }
  
}
