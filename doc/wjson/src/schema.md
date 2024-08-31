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

## Special JSON fields
1. `$schema`: 对 JSON 元素，可以指定该元素的 JSON schema URI，通过该 URI 获取到 schema 信息，适合于：
   - top level JSON element
   - dynamic inner level JSON element
   诸如 idea/visual code 之类的编辑器可以通过该 URI 获取到 schema 信息，从而提供更好的提示和校验。
2. `$version`: 对应于 @js.version
3. `$enum`， 为 SUM 类型生成的 tag，非 JSON Schema 标准字段
4. `$or`, `$value`, 为 OR 类型生成的 tag，非 JSON Schema 标准字段


