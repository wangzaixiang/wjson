package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.{*, given}

import scala.collection.mutable

class TestJsonInterpolation extends AnyFunSuite {

  test("basic string interpolation") {

    val name = "John"
    val age = 30
    val city = "New York"
    val ford = "Ford"
    val bmw = "BMW"

    val js1 = json"""{"name":${name},"age":${age},"cars":[${ford},${bmw},"Fiat"],"city":${city}}"""

    assert(js1 == JsObject(
      "name" -> "John",
      "age" -> 30,
      "cars" -> List("Ford", "BMW", "Fiat"),
      "city" -> "New York"
    ))

    val x: JsValue = List("a", "b", "c")
    assert(x == List("a", "b", "c").toJson)

  }

  test("interpolation with insert sapces at boundary") {
    val name = "John"
    val age = 30
    val city = "New York"
    val ford = "Ford"
    val bmw = "BMW"

    val parts = Array("""@{@"name"@:@""",
      """@,@"age"@:@""",
      """@,@"cars"@:@[@""",
      """@,@""",
      """@,@"Fiat"@]@,@"city"@:@""",
      """@}@""")
    val args = Seq( name: JsValue, age: JsValue, ford: JsValue, bmw: JsValue, city: JsValue)

    val patten = "@".r
    val spaces = " \t\n\r"
    def cleaned(parts: Array[String]): Array[String] = parts.map( _.replaceAll("@", "") )

    def randSpaces(n: Int): String =
      val sb = new StringBuilder
      (1 to n).foreach: _ =>
        val r = scala.util.Random.nextInt( Math.abs(System.currentTimeMillis().toInt) ) % spaces.length
        sb.append( spaces.charAt(r) )
      sb.toString


    def randRelaceSplace(s: String) = patten.replaceSomeIn( s, m =>
      val random = scala.util.Random.nextInt(20)
      if random <= 5 then Some("")
      else Some( randSpaces(random) )
    )

    def randReplaceSpaces(parts: Array[String]) = parts.map( randRelaceSplace )

    val expect = new StringContext(cleaned(parts): _*).json(args: _*)

    (1 to 1000).foreach: _ =>
      val parts2 = randReplaceSpaces(parts)
      val result = new StringContext(parts2: _*).json(args: _*)
      assert(result == expect)

  }

}
