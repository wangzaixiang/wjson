# JSON Schema 设计草案

## Design Goals
1. 使用 scala ADT 类型进行 JSON 建模，用于描述数据结果，可以自动生成 JSON Schema.
2. 通过 @js.description 之类的 annotation 定义更多的 constraint.

## Usage
1. via macro during compile time
   ```scala
      val schema: String = JsonSchema.of[T]
      println(schema)
   ```

2. running generator on tasty file
    ```
    //> using lib wjson.schema
    import wjson.schema.*
   
    JsonSchemaGenerator.generate("path/to/tasty/file.tasty", "path/to/output/schema.json")
    ```
   
## JSON Schema Annotations
1. @js.description
2. @js.enums
3. @js.multipleOf
4. @js.maximum
5. @js.minimum
6. @js.exclusiveMaximum
7. @js.exclusiveMinimum
8. @js.maxLength
9. @js.minLength
10. @js.pattern
11. @js.maxItems
12. @js.minItems
13. @js.uniqueItems
14. @js.maxContains
15. @js.minContains
16. @js.maxProperties
17. @js.minProperties
18. @js.format
    - date/time/datetime/duration
    - email
    - hostname
    - ipv4/ipv6
    - uri
    - uuid
19. @js.open   mark a type `additionalProperties`= true
20. required: all types marks required except for `Option[T]` or `T|Null`

## Special JSON fields
1. `$schema`: 对 JSON 元素，可以指定该元素的 JSON schema URI，通过该 URI 获取到 schema 信息，适合于：
   - top level JSON element
   - dynamic inner level JSON element
   诸如 idea/visual code 之类的编辑器可以通过该 URI 获取到 schema 信息，从而提供更好的提示和校验。
   
2. `$tag` 非 JSON Schema 标准字段
   - 对 enum 类型，可以自动添加 `$tag` 字段，取 case 名作为值。
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
   - 对 or 类型
     1. `T | Null`: 理解为可以为 Null 的类型， 不会为 T 生成 tag, 与 `Option[T]` 是类似的
     2. `Option[T]` 的 tag 与 `T` 是一样的
     3. 如果 T 是 基础类型：`Int | Long | String | Boolean | Double ｜ Float | Null `，则不会生成 tag，也包括对应的 `Option[T]`
     4. 对非基础类型， 取该类型的名称作为 tag
        1. 非容器类型(非范型)。 如 `demo2.Bean1`
        2. 对容器类型，如 `scala.collection.immutable.List[demo2.Bean2]`
     5. 如果包含 tag, 那么 JSON 型如：
        ```json
          {
            "$tag": "demo2.Bean1",
            "$value": {
                "name": "hello",
                "age": 10
            }
          } 
        ```

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

