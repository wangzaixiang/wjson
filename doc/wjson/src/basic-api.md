# basic JSON API

## JSON 基本类型API
```scala 3
enum JsVal:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double)
    case JsString(value: String)
    case JsArray(elements: List[JsVal])
    case JsObject(fields: Seq[String, JsVal])
```
JsObject 会保留构建时字段的顺序，在格式化输出时，保留该顺序。但 equals 方法会忽略字段的顺序。


## JSON对象
wjson 提供了多种方式来构造 JSON 值。
```scala 3
import wjson.{*, given}

// JSON Object: {"name":"John","age":18} 下面的多种方式，都可以构造出同一个 JSON 对象
val objStr = """{"name":"John","age":18}"""
val obj1 = JsObject(Seq("name" -> JsString("John"), "age" -> JsNumber(18))) // 使用 JsObject 构造器
val obj2 = JsObject(Seq("name" -> "John", "age" -> 18)) // 通过隐式转换，将基本类型转换为 JsValue
val obj3 = JsValue.obj("name" -> "John", "age" -> 10) // 使用 JsValue.obj 构造 JsObj, 更简洁

val obj4 = JsValue.parseJson(objStr)    // 作为 JSON 字符串解析
val obj5 = JsValue.parseJson5(objStr)   // 作为 JSON5 字符串解析

val obj6 = json"""{"name":"John","age":18}"""  // 使用 json"..." 字符串插值
val obj7 = json5"{name:'John', age:18 }"  // 使用 json5"..." 字符串插值， 注意 JSON5 的语法更宽松

// JSON Array: [1,2,3]
val arr1 = JsArray(Seq( JsNumber(1), JsNumber(2), JsNumber(3) )) // 最基本的构造方式
val arr2 = JsArray(Seq(1,2,3)) // 通过隐式转换，将基本类型转换为 JsValue
val arr3 = JsValue.arr(1,2,3) // 使用 JsValue.arr 构造 JsArray, 更简洁

val arr4 = JsValue.parseJson("[1,2,3]")    // 作为 JSON 字符串解析
val arr5 = JsValue.parseJson5("[1,2,3, ]")   // 作为 JSON5 字符串解析， JSON5 支持末尾的逗号

val arr6 = json"[1,2,3]"  // 使用 json"..." 字符串插值
val arr7 = json5"[1,2,3, ]"  // 使用 json5"..." 字符串插值， 注意 JSON5 的语法更宽松
```

## 基本 API
1. 输出 JSON 字符串
    ```scala 3
      js1.show // 输出 JSON 字符串
      js1.showPretty // 输出格式化的 JSON 字符串
      js1.show(indent = 4) // 输出格式化的 JSON 字符串，缩进为4个空格
    ```

2. JsObject 操作
    ```scala 3
      obj1.field("name")  // 类似于 Map 的 apply 方法，获取字段值
      obj1.filedOpt("name") // 类似于 Map 的 get 方法，获取字段值，返回 Option
      obj1.contains("name") // 判断是否包含某个字段
      obj1.keys // 获取所有的字段名

      obj1 ++ obj2 // 合并两个 JsObject, 如果有重复的字段，后者覆盖前者
      obj1 ++ Seq("name" -> "Tom", "age" -> 20) // 合并 JsObject 和 Map[String, Any]
      obj1 + ("name" -> "Tom") // 添加一个字段
   
      obj1.remove("field1", "field2") // 删除字段
    ```

3. JsArray 操作
   ```scala 3
      arr1.elements // 获取所有的元素
      arr1(0) // 获取第一个元素
   
      arr1 ++ arr2 // 合并两个 JsArray
      arr1 ++ Seq(1,2,3) // 合并 JsArray 和 Seq[T]
      arr1 :+ 4    // 在尾部添加一个元素，返回新的 JsArray
      0 +: arr1    // 在头部添加一个元素，返回新的 JsArray
   
      arr1.updated(1, 100) // 更新第二个元素的值为 100，返回新的 JsArray
   ```

4. JsValue 操作
   ```scala 3
      js1 match
        case JsNull => // JsNull 的模式匹配
        case JsBoolean(b) => b // JsBoolean 的模式匹配
        case JsNumber(n: Long) => n // JsNumber 的模式匹配
        case JsNumber(n: Double) => n // JsNumber 的模式匹配
        case JsString(s) => s // JsString 的模式匹配
        case JsObj(fields) => fields // JsObj 的模式匹配
        case JsArr(elements) => elements // JsArr 的模式匹配

      js1.isObj // 判断是否是 JsObj
      js1.isArr // 判断是否是 JsArr
      js1.isNull // 判断是否是 JsNull
      js1.isBoolean // 判断是否是 JsBoolean
      js1.isNumber // 判断是否是 JsNumber
      js1.isString // 判断是否是 JsString
   
      js1 == js2  // 判断是否相等, 对 JsObject, 会忽略字段的顺序
   
      js1.asBool // 强制转换为 JsBoolean，如果类型不匹配，会抛出异常
      js1.asNum // 强制转换为 JsNumber，如果类型不匹配，会抛出异常
      js1.asStr // 强制转换为 JsString，如果类型不匹配，会抛出异常
      js1.asObj // 强制转换为 JsObject，如果类型不匹配，会抛出异常
      js1.asArr // 强制转换为 JsArray，如果类型不匹配，会抛出异常
   ```   
   
5. JSON 与 对象值之间的转换
    ```scala 3
     case class Person(name: String, age: Int) derives JsValueMapper // 通过 derives JsValueMapper 自动生成 JSON 映射
     case class User(name: String, age: Int) // 未定义 JsValueMapper 也可以，但每次映射都会生成 JsValueMapper 实例, 会增加编译后的代码大小
     val js1 = json"""{"name":"John","age":18}"""
   
     val person = js1.toBean[Person]   // 反序列化为 Person
     val user = js1.toBean[User]       // 反序列化为 User
     val js2 = person.toJson           // 序列化为 JsValue
    ```
   
6. 字符串插值
   ```scala 3
      val name = "John"
      val age = 18
      val obj1 = json"""{"name": $name, "age": $age}"""  // 使用 json"..." 字符串插值
      val obj2 = json5"{name: $name, age: $age }"  // 使用 json5"..." 字符串插值， 注意 JSON5 的语法更宽松

      val arr1 = json"[1,2,3, $age]"  // 使用 json"..." 字符串插值
      val arr2 = json5"[1,2,3, $age,  ]"  // 使用 json5"..." 字符串插值， 注意 JSON5 的语法更宽松
      ```
   字符串插值也可以用于进行简单的模式匹配：
   ```scala 3
      val js = json"{name: 'John', age: 18, }"

      jsval match
        case json"{name: $name, age: $age}" => println(name, age)  // John 18
        case _ => println("not match")
      
      json5"{ name: 'John', age: 18, address: { city: 'Beijing', country: 'China' }, scores: [80,90,100] }" match 
        case json5"{ address: {city: $city}, scores:[80,$score1,$score2]}" => println(city, score1, score2)  // Beijing 90 100
        case _ => println("not match")
      
   ```
   
   使用字符串插值进行模式匹配时，提供了一定的灵活性，例如：
   1. 对 JsObject, 在模式中可以仅匹配部分字段，而忽略其他字段。
   2. 对 JsArray, 可以匹配前面部分元素，而忽略其他元素。
   
   需要更强大的模式匹配功能，可以使用 wjson-pattern 模块。
   
