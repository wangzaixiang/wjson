# ADT support

## Primitive Types

## Container Types

## Product Type
wjson 支持 Product 类型的 JSON 序列化和反序列化。
```scala 3
      case class Bean(name: String, age: Int)
```
对应的 JSON 如下：
```json
    {
      "name": "wang",
      "age": 18
    }
```

## Sum Type
对 enum 类型，可以自动添加 `$tag` 字段，取 case 名作为值。
```scala 3
      enum Color:
         case Red, Green, Blue
         case Mixed(r: Int, g: Int, b: Int, alpha: Int)
     
```
1. 对 simple case, 如 Red, Green, Blue，编码为 string，无需 tag
2. 对 product case， 如 Mixed，编码为：
```json 
        {
          "$tag": "Mixed",
           "r": 10, "g": 20, "b": 30, "alpha": 255
        }
```

- 读取更高版本的JSON时，可能会存在不认识的 tag, 会忽略该枚举值。

## Or Type
wjson 支持 `X | Y` 的 Or Type 的 JSON 序列化和反序列化。

示例：
```scala 3
       case class Root(bean: Bean1 | List[Bean2] | Array[String]) 
```
对应的 JSON 如下：
```json
    [{
       "bean":{
            "$or":"demo2.Bean1",
            "$value":{"name":"wang","age":18}
       }
    },
    {
      "bean":{
            "$or":"scala.collection.immutable.List[demo2.Bean2]",
            "$value":[ {"name":"zhang","age":20} ]
      }
    },
    {
      "bean": {
            "$or":"scala.Array[java.lang.String]",
            "$value":["hello","world"]
      }
    }]
```

Or Type 的成熟度不如 enum 的支持, 其规则相对比较复杂:
1. `T | Null`: 理解为可以为 Null 的类型， 与 `Option[T]` 是类似的
2. 如果有多个子类型，会生成一个 `$or` 的元素，指定其实际类型，使用 `$value` 作为值。
    - 基础子类型：`Int | Long | String | Boolean | Double ｜ Float | Null `，不会生成 tag
    - 对非基础类型， 取该类型的名称作为 tag
        1. 非容器类型(非范型)。 如 `demo2.Bean1`
        2. 对容器类型，如 `scala.collection.immutable.List[demo2.Bean2]`
        3. 需要注意，这里的 tag 与 scala 语言有绑定，未来考虑转化为一个语言中立的 tag。
    - 如果只有一个子类型需要 tag, 则会优化为不生成 $or
3. 不能有 `List[A] | List[B]` 这样的类型，因为Java的擦除式范型，无法通过类型检测数据的实际类型。
    - 特殊支持： 可以支持 `Option[A] | Option[B]`
4. `Option[T]` 与 `Null` 不能同时存在。

使用 Or Type 时，因为要考虑的情况相对复杂，建议尽可能使用 enum 来替代。

## JSON Pointer support
```scala
case class LocalPointer[T](p: String)  // ref inside the same document
case class GlobalPointer[T](p: String) // ref outside document

case class Relation(srcView: LocalPointer[View], destView: LocalPointer[View])

val relation = Relation("view1", "view2")
```

```json5
{ 
  "srcView": "view1", 
  "destView": "view2"
}
```
