# wJson: 一个简单、直观的Scala JSON库

wJson 是一个Scala3 JSON库，它提供了一个简单的、直观的API，用于在Scala中处理JSON。wJson的设计参考了如下的库，并试图变得提供更为
简单的API：
- [Spray JSON](https://github.com/spray/spray-json)
- [wangzaixiang's fork of Spray JSON](https://github.com/wangzaixiang/spray-json)
- [ujson](https://www.lihaoyi.com/post/uJsonfastflexibleandintuitiveJSONforScala.html)

wJson 基于scala3，也是我学习Scala3的一个实践项目，通过 wJson，全面熟悉一下 scala3 的语法，以及新的 Macro 系统的使用。

# 安装

以 sbt 为例：
```scala
libraryDependencies += "com.github.wangzaixiang" %% "wjson" % "0.1.0"
```

# 使用

在使用 wjson 库之前，简单引入一下：

```scala

```

1. JSON 基本类型API
   ```scala
   enum JsVal:
     case JsNull
     case JsBoolean(value: Boolean)
     case JsNumber(value: Double)
     case JsString(value: String)
     case JsArr(elements: List[JsVal])
     case JsObj(fields: Map[String, JsVal])
   ```
   > 使用 enum + ADT 对数据结构进行建模，是一种很简洁的实现方式。
2. 解析JSON字符串、构造JsVal
   ```scala
   val jsonStr = """{"name":"John","age":18}"""
   val jsval0 = JsParser.parse( JsonInput(jsonStr) )  // 使用 JsonParser 进行 JSON 解析
   val jsval1: JsVal = JsVal.parse(jsonStr)    // 使用 JsVal.parse API
   val jsval2: JsVal = jsonStr.parseJson       // 使用扩展方法 String.parseJson
   
   val jsval3 = json"""{"name":"John","age":18}"""  // 使用 json"..." 字符串插值
   val jsval4 = JsObj(Map("name" -> JsString("John"), "age" -> JsNumber(18))) // 使用 JsObj 构造器
   val jsval5 = JsObj("name" -> JsString("John"), "age" -> JsNumber(18))  // 使用 JsObj 构造器
   val jsval6 = JsObj("name" -> "John", "age" -> 18)  // 使用 JsObj 构造器
   

   assert( jsval1 == jsval0 && jsval2 == jsval0 && jsval3 == jsval0 && jsval4 == jsval0 
      && jsval5 == jsval0 && jsval6 == jsval0 )
   ```
   > 这7中方式，都可以解析出同一个 JSON 对象。

3. 序列化与反序列化
   ```scala
   
   case class Person(name: String, age: Int)
   val jsval = json"""{"name":"John","age":18}"""
   
   val person = jsval.to[Person] // jsval 具有扩展方法 to[T] 可以反序列化为 T（必须是case class）
   val jsval2 = person.toJson    // case class 具有扩展方法 toJson 可以序列化为 JsVal
   
   ```
   
   wjson 的序列化和反序列化功能，它支持：
   - 基本类型的序列化，包括 Byte、Short、Char、Int、Long、Float、Double、Boolean、String 的序列化
   - BigInt、BigDecimal
   - Option[T]: 其中 T 必须是可序列化的（递归游戏）
   - List[T]、Seq[T]、Set[T]、Map[String, T], Array[T]: 其中 T 必须是可序列化的.（递归游戏）
   - case class: 其中所有的字段必须是可序列化的（递归游戏）

   只要符合上述的类型规则，那么就可以使用 wjson 的API： `jsVal.to[T]`、`(value:T).toJson`  来进行对象的序列化、反序列化，
   而无需编写任何的代码。相比 spray-json 来说，wjson的API要简单很多，功能要强大很多（不再需要编写）。

4. json 插值能力
   ```scala
   val jsonStr1 = json"""{"name":"John","age":18}"""  // 标准 JSON 语法
   val jsonStr2 = json"{name: 'John', age:18, } "  // 扩展 JSON 语法
   
   val name = "John"
   val age = 18
   val jsonStr3 = json"{name: $name, age: $age}"  // 扩展 JSON 语法 + 插值
   ```
   json"..." 字符串插值，可以让我们更加简洁的构造 JSON 对象。其除了支持标准的 JSON 语法之外，还支持扩展的javascript语法，
   包括：
   1. 字段名，可以直接使用 name,age 等标识府，而无需加引号。（javascript语法）
   2. 字符串，可以直接使用单引号，或者双引号。（javascript语法）
   3. 数组、对象的最后一个成员后，可以添加逗号。（javascript语法）
   4. 使用${} 来表示插值，嵌入动态的内容。
   
   json插值除了可以构造一个 JsVal 对象之外，还可以作为 Pattern 来使用，用于匹配一个JsVal，并从中提取出某些值。
   ```scala
    val jsval1 = json"{name: 'John', age: 18, }"
   
    jsval match {
      case json"{name: $name, age: $age}" => println(name, age)  // John 18
      case _ => println("not match")
    }
   
    json"{ name: 'John', age: 18, address: { city: 'Beijing', country: 'China' }, scores: [80,90,100] }" match {
      case json"{ address: {city: $city}, scores:[80,$score1,$score2]}" => println(city, score1, score2)  // Beijing 90 100
      case _ => println("not match")
    }
   
   ```
   使用 json 的插值匹配功能，可以一次性匹配一个复杂的JSON 对象，并且按照结构提取出某些值。这可以在不进行JSON反序列化的情况下，快速的
   对JSON进行匹配、字段提取、数据验证工作。

5. wjson 还计划提供一个更为强大的 JSON 模式匹配语言，这个目前尚且在设计之中，将在外下一个版本中提供。