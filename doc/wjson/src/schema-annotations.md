## JSON Schema Annotations

可以在 ADT 申明中，使用如下的 annotation 附加JSON Schema的元信息。

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

## 扩展
1. @js.toplevel 
   
    表示改类型可以作为文档顶层元素，顶层元素在定义时，可选的支持 `$schema` 属性，用于指定 JSON Schema 的定义。
    
    tips: 可以在 JSON 文件中，手动指定 $schema, idea 编辑器会自动获取 schema 对文档进行校验
   （不在需要额外配置 mapping）

2. @js.version 
3. @js.open:   mark a type `additionalProperties`= true
4. @js.dynamic