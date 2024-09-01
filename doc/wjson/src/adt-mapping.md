# Bean/JSON Mapping

wjson 提供了丰富的 Bean/JSON 映射支持：
```scala 3
    true.toJson  // JsBoolean(true)
    JsBoolean(true).to[Boolean]  // true

    123.toJson  // JsNumber(123)
    JsNumber(123).to[Int]  // 123

    "hello".toJson  // JsString("hello")
    JsString("hello").to[String]  // "hello"

    case class Person(name: String, age: Int)
    val p = Person("John", 18)
    p.toJson  // JsObject("name" -> JsString("John"), "age" -> JsNumber(18))
    json"""{ "name": "John", "age": 18 }""".to[Person]  // Person("John", 18)
```

1. 基本类型的序列化，
   - Byte, Short, Int, Long, Float, Double, BigDecimal(scala, java), BigInt(scala, java)
   - Boolean
   - String 
2. `Option[T]`: 其中 T 必须是可序列化的(T: JsValueMapper)
3. 集合类型
   - `List[T]`
   - `Seq[T]`
   - `Set[T]`
   - `SortedSet[T]`
   - `Map[String, T]`
   - `Map[K, V]` (K != String) 会映射为 `List[(K,V)]` 对应的 JSON 结构
   - `Array[T]`
   集合元素的类型 T 必须是可序列化的(T: JsValueMapper)
4. [ ] Tuple 类型， 目前尚未支持，计划在后续版本中支持
5. case class: 其中所有的字段必须是可序列化的.
   1. 如果字段类型 T 是 `Option[T1]`，则该字段是可选的，如果 JSON 中没有该字段，则使用 `None` 填充
   2. 如果字段有默认值，则该字段是可选的，如果 JSON 中没有该字段，则使用默认值填充。
   3. 所有其他的字段都是必须的。如果JSON 中没有该字段，则抛出异常。(不会对应于 null 值)
   4. case class 与 JSON 的映射关系定义，请参考：[JSON Schema for ADT](./schema-adt.md)
6. enum class: 其中所有的字段必须是可序列化的.
   1. enum class 与 JSON 的映射关系定义，请参考：[JSON Schema for ADT](./schema-adt.md)
   

如果你的类型 T 不符合上述规则，但你又希望提供序列化、反序列化的能力，你可以为之提供一个 `JsonValueMapper[T]` 的隐式值。
```scala 3
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