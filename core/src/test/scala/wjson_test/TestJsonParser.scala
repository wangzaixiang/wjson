package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsonParser.ParsingException
import wjson.{*, given}

class TestJsonParser extends AnyFunSuite {

  test("Json Parser") {
    val jsonStr = """{"name":"John","age":30,"cars":["Ford","BMW","Fiat"],"city":"New York"}"""
    val jsval = jsonStr.parseJson

    assert(jsval == JsObject(
      "name" -> "John",
      "age" -> 30,
      "cars" -> List("Ford", "BMW", "Fiat"),
      "city" -> "New York"
    ))

  }

  // insert random spaces into json string, and it should parse the same
  test("insert spaces at boundary") {
    val jsonStr = """@{@"name"@:@"John"@,@"age"@:@30@,@"cars"@:@[@"Ford"@,@"BMW"@,@"Fiat"@]@,@"city"@:@"New York"@}@"""
    val clean = jsonStr.replaceAll("@", "").nn
    val expect = clean.parseJson

    val pattern = "@".r

    val spaces = " \t\n\r"
    def randStr(len: Int): String = {
      val sb = new StringBuilder
      (1 to len).foreach { _ =>
        val r = scala.util.Random.nextInt( Math.abs(System.currentTimeMillis().toInt)  ) % spaces.length
        sb.append(spaces.charAt(r))
      }
      sb.toString
    }

    (1 to 1000).foreach { i =>
      val result = pattern.replaceSomeIn(jsonStr, m =>
        val random = scala.util.Random.nextInt(20)
        if random <= 5 then Some("")
        else Some( randStr(random) )
      )

      val jsval = result.parseJson
      assert(jsval == expect)
    }

  }

  test("insert non spaces at boundary"){
    val jsonStr = """@{@"name"@:@"John"@,@"age"@:@30@,@"cars"@:@[@"Ford"@,@"BMW"@,@"Fiat"@]@,@"city"@:@"New York"@}@"""

    val pattern = "@".r

    val nonSpaces = """aA[{}]()~!@#$%^&*-_+=\|;:"'<,>./?'"""
    def randStr(): String =
      val r = scala.util.Random.nextInt( Math.abs(System.currentTimeMillis().toInt)  ) % nonSpaces.length
      "\\" + nonSpaces.substring(r, r+1)

    (1 to 1000).foreach { i =>
      val result = pattern.replaceSomeIn(jsonStr, m =>
        val random = scala.util.Random.nextInt(20)
        if random <= 5 then Some("")
        else Some( randStr() )
      )

      assertThrows[ParsingException]( result.parseJson )
    }

  }

}
