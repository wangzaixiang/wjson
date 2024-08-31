# basic JSON API

## JSON 基本类型API
```scala 3
enum JsVal:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double)
    case JsString(value: String)
    case JsArr(elements: List[JsVal])
    case JsObj(fields: Map[String, JsVal])
```
使用 enum + ADT 对数据结构进行建模，是一种很简洁的实现方式。

## 解析JSON字符串、构造JsVal
```scala 3
val jsonStr = """{"name":"John","age":18}"""
val jsval0 = JsParser.parseJson( JsonInput(jsonStr) )  // 使用 JsonParser 进行 JSON 解析
val jsval1: JsVal = JsVal.parseJson(jsonStr)    // 使用 JsVal.parse API
val jsval2: JsVal = jsonStr.parseJson       // 使用扩展方法 String.parseJson

val jsval3 = json"""{"name":"John","age":18}"""  // 使用 json"..." 字符串插值
val jsval4 = JsObj(Map("name" -> JsString("John"), "age" -> JsNumber(18))) // 使用 JsObj 构造器
val jsval5 = JsObj("name" -> JsString("John"), "age" -> JsNumber(18))  // 使用 JsObj 构造器
val jsval6 = JsObj("name" -> "John", "age" -> 18)  // 使用 JsObj 构造器

assert( jsval1 == jsval0 && jsval2 == jsval0 && jsval3 == jsval0 
  && jsval4 == jsval0 && jsval5 == jsval0 && jsval6 == jsval0 )
```
这7中方式，都可以解析出同一个 JSON 对象。

## 序列化与反序列化
```scala 3
case class Person(name: String, age: Int)
val jsval = json"""{"name":"John","age":18}"""

val person = jsval.to[Person] // jsval 具有扩展方法 to[T] 可以反序列化为 T（必须是case class）
val jsval2 = person.toJson    // case class 具有扩展方法 toJson 可以序列化为 JsVal
```

wjson 的序列化和反序列化功能，它支持：
1. 基本类型的序列化，包括 Byte、Short、Char、Int、Long、Float、Double、Boolean、String 的序列化
BigInt、BigDecimal
2. Option[T]: 其中 T 必须是可序列化的（递归游戏）
3. List[T]、Seq[T]、Set[T]、Map[String, T], Array[T]: 其中 T 必须是可序列化的.（递归游戏）
4. case class: 其中所有的字段必须是可序列化的（递归游戏）
5. enum class: 其中所有的字段必须是可序列化的（递归游戏）

如果你的类型 T 不符合上述规则，但你又希望提供序列化、反序列化的能力，你可以为之提供一个 `JsonValueMapper[T]` 的隐式值。
```scala
  // support Map[K,V] mapping to List[(K,V)]
  given [K: JsValueMapper, V: JsValueMapper]: JsValueMapper[Map[K, V]] = mapMapping2[K, V]
  def mapMapping2[K: JsValueMapper, V: JsValueMapper]: JsValueMapper[Map[K, V]] = new JsValueMapper[Map[K, V]]:
    def fromJson(js: JsValue): Map[K, V] = (js: @unchecked) match
      case o: JsArray =>
        o.elements.map:
          case el: JsObject =>
            val key = summon[JsValueMapper[K]].fromJson(el.field("key"))  // expect key field
            val value = summon[JsValueMapper[V]].fromJson(el.field("value")) // expect value field
            key -> value
          case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
        .toMap

      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")

    def toJson(t: Map[K, V]): JsValue =
      val entry2Json = (k: K, v: V) => JsObject( "key" -> summon[JsValueMapper[K]].toJson(k), "value" -> summon[JsValueMapper[V]].toJson(v) )
      val entries = t.toList.map { case (k, v) => entry2Json(k,v) }
      JsArray( entries: _* )

```
只要符合上述的类型规则，那么就可以使用 wjson 的API： 
1. `jsVal.to[T]`  反序列化
2. `(value:T).toJson` 序列化 

而无需编写任何的代码。相比 spray-json 来说，wjson的API要简单很多，功能要强大很多（不再需要编写protocol, reader, writer等，也不再有22个参数的限制等），
**write less, get more**

