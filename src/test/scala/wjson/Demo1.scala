package wjson

@main
def demo1() =

  val src: JsValue = json"""{
      name: "John",
      age: 30
    }"""

  // sc.json.apply(a,b)

  // JsVal("name", "John")

  val js = src.toString.parseJson

  val rejson"""{
      name: ${name:String} @ string,
      ${age: Int} @ age: int,  # age field as int
      ${price: JsValue} @ order/price,   # order.price field
      ${others} @ _*,   # all other fields
      arr: [ name @ 1, 2, 3, _* ]   # 无序数组
      arr2: ( 1, 2, 3, _* )  # 有序数组
    }""" = src

  println(s"name = $name, age = $age price=$price, others=$others")

  case class User(name: String, age: Int = 10, student: Student)
  case class Student(name:String = "wangzx", age: Int)

  val user = User(name, age, Student("John", 20))
//  val student = Student(name, age)

//  println( "user.toJson: "  + user.toJson )
  println(" user = " + JsNull.to[User])


