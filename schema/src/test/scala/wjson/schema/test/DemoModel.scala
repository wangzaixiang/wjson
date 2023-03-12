package wjson.schema.test


case class Person(name: String, age: Int, color: Color)

enum Color(val r: Int, val g: Int, val b: Int):
  def toInt: Int = (r << 16) | (g << 8) | b

  case Red extends Color(255, 0, 0)
  case Green extends Color(0, 255, 0)
  case RGB(override val r: Int, override val g: Int, override val b: Int) extends Color(r, g, b)
  case Blue extends Color(0, 0, 255)