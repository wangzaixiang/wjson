# json 字符串插值
```scala
val jsonStr1 = json"""{"name":"John","age":18}"""  // 标准 JSON 语法
val jsonStr2 = json5"{name: 'John', age:18, } "  // 扩展 JSON 语法

val name = "John"
val age = 18
val jsonStr3 = json5"{name: $name, age: $age}"  // 扩展 JSON 语法 + 插值
```
`json"..."` 字符串插值，可以让我们更加简洁的构造 JSON 对象。
其除了支持标准的 JSON 语法之外，还可以使用 `json5""`还支持扩展的 JSON5语法， 包括：
- 字段名，可以直接使用 name,age 等标识府，而无需加引号。（javascript语法）
- 字符串，可以直接使用单引号，或者双引号。（javascript语法）
- 数组、对象的最后一个成员后，可以添加逗号。（javascript语法）
- 其他的 JSON5 特性，如注释等。

使用字符串插值，还有一个特点是：JSON 格式是在编译期完成的，这样可以避免运行时错误，并提高运行性能。

在字符串插值中，使用`${}` 来表示插值，嵌入动态的内容。
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
使用 json 的插值匹配功能，可以一次性匹配一个复杂的JSON 对象，并且按照结构提取出某些值。这可以在不进行JSON反序列化的情况下，快速的 对JSON进行匹配、字段提取、数据验证工作。

当然， wjson-pattern 通过提供一个更强大的DSL语言，可以提供更为强大的模式匹配能力，这个可以参考 [wjson-pattern](pattern.md)。
