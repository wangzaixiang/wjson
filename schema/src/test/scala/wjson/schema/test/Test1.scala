package wjson.schema.test

import wjson.*


object Test1 {

  def main(args: Array[String]): Unit =
    val color1: Color = Color.Red
    val color2: Color = Color.RGB(255, 0, 0)
    val json1 = color1.toJson
    val json2 = color2.toJson

    println("json: " + json1 + "\njson2 = " + json2)

}
