# wJson

## usage

see [Usage](doc/wjson.md)

## Build

# Tasks
-[X] cpmiles with explicit nulls -Yexplicit-nulls
-[X] make JsonPattern & RejsonInterpolation a submodule "wjsonPattern"
-[ ] JsonValueMapper 测试用例完善，尽可能完备的支持 ADT，支持 OrType，支持 循环引用
-[X] 支持 [JSON5](https://spec.json5.org/#grammar) 格式， parseJson(true) -> parseJson5 
-[ ] 完善 schema 项目，补充测试用例

# wjson.core
module wjson.core provides:
1. JsValue types
2. Basic implementations for `JsValueMappper[T]`
3. a fast json parser and pretty printer

# wjson.json5
-[X] 支持 [JSON5](https://spec.json5.org/#grammar) 格式

# wjson.pattern
一个独立的 JSON Pattern Matcher, 相关文档参考：[wjson模式匹配简介](https://zhuanlan.zhihu.com/p/538029804)

将现有项目中的 JsonPattern & RejsonInterpolation 移动到这个子项目中

# wjson.adt
wjson.adt 项目提供是 Algebraic Data Type 的支持:
- [ ] ADT <-> JSON
  - OrType
  - 循环引用
- [ ] ADT -> JSON Schema 根据 ADT 生成对应的 Schema
- [ ] ADT -> protobuf 低优先级
- [ ] ADT -> thrift 低优先级

wjson.adt 项目的目标是为 ADT 提供更大的价值，可以用于：
1. 使用 ADT 来描述数据结构， 通过生成对应的 Schema，可以使用 JSON 进行数据的描述、校验，以及编辑态工具支持
2. 使用 ADT 替代 IDL 来描述接口，生成 Schema 作为接口的文档、数据校验，以及后续 stub 代码的生成。

## ADT -> [protobuf](https://protobuf.dev/programming-guides/proto3/#json) 
基本上，ADT是覆盖了 protobuf 的能力的。理论上可以进行双向的转换。
1. [data type](https://protobuf.dev/programming-guides/proto3/#scalar)
    - message: object
    - enum: string
    - Map<K,V>: object, support only K = string
    - repeated: array
    - bool/string/int32/int64/float/double/uint32/uint64/fixed32/fixed64 mapping
    - bytes: base64 string
    - Timestamp: string( "yyyy-mm-ddThh:mm:ss.SSSZ" )
    - Duration: string( "1s" )
    - Struct: object
2. service
3. optional: mapping to Option[T] or T|Null
4. label: using @label annotation
5. default valus: mapping to field's default value
6. oneof: mapping to OrType