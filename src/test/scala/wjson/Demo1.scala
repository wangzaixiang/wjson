package wjson

@main
def demo1() =

  val src: JsVal = json"""{
      name: "John",
      age: 30
    }"""

  // sc.json.apply(a,b)

  // JsVal("name", "John")

  val js = src.toString.parseJson

  val rejson"""{
      name: ${name:String} @ string,
      ${age: Int} @ age: int,  # age field as int
      ${price: JsVal} @ order/price,   # order.price field
      ${others} @ _*,   # all other fields
      arr: [ name @ 1, 2, 3, _* ]   # 无序数组
      arr2: ( 1, 2, 3, _* )  # 有序数组
    }""" = src

  // sc.rejson.unapply(src) -> Option[ List(name, age, price, others) ] match {
  // case (name: String, age:Int, ..) =>
  // case _ => throw MatchError

  println(s"name = $name, age = $age price=$price, others=$others")

  case class User(name: String, age: Int)
  class Student(name:String, age: Int)

  val user = User(name, age)
//  val student = Student(name, age)

  println( "user.toJson: "  + user.toJson )
  println(" user = " + JsVal.JsNull.to[User])

  // println("student.toJson = " + student.toJson)

