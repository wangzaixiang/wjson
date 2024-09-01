# 简介

wjson 是一个 Scala 3 的 JSON 处理库，提供了基本的 JSON 构建、解析、操作的功能，并提供了对 ADT 数据类型的 JSON 映射支持，

1. wjson-core 提供了基本的 JSON 操作 API
   - JsValue API： 对 JSON 数据结构的 ADT 抽象，支持 immutable 风格的 build, access, show 等操作，是 wjson 的核心数据结构。
   - JSON parer, 一个高性能的JSON解析器，支持标准的 JSON 语法
   - JSON5 parser, JSON5 语法解析器。[JSON5](https://json5.org) 是 JSON 的超集，支持注释、多行字符串等特性。
   - json"..." 和 json5"..." 字符串插值
   - ADT 支持
     - product type (case class)
     - sum type (enum)
     - or type (union)
2. wjson-pattern 一个实验性的 JSON 模式匹配库，非常适合于从 JSON 中快速提取信息。
   - jsp"..." 提供了一个 JSON Pattern DSL 语言，可以匹配复杂的 JSON 数据结构
   - 匹配 JSON 数据结构 并 提取数据
3. wjson-schema 一个实验性的 [JSON Schema](https://json-schema.org) support 库
   - 对 ADT 类型生成 JSON Schema
   - 提供一组 annotation 用于定义 JSON Schema 的约束

## why another JSON library?
1. 更好的 ADT 支持， 相比其他的 JSON 库，几乎不用编写代码，就可以处理 Case class / enum 的 JSON 映射。
   (对比 spray-json，wjson 的 API 更简单、更强大)
2. 强大的 interpolation 支持.
3. JSON5 支持。JSON5 是 JSON 的超集，支持注释、多行字符串等特性，更适合于配置文件等场景。
4. JSON Schema 支持，通过 ADT 类型生成 JSON Schema，提供更好的数据校验和提示。
5. 当然，wjson 也是我学习 Scala 3 的一个练手项目，尤其是对 Scala3 的 Macro 的使用。 wjson 广泛的使用 Macro 来提供一个简单、强大的 API。

## TODO List
- [ ] JsValueMapper for Tuple