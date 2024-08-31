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
19. @js.open   mark a type `additionalProperties`= true
20. required: all types marks required except for `Option[T]` or `T|Null`
