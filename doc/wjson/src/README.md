# 简介

wjson 是一个 Scala 3 的 JSON 处理库，提供了基本的 JSON 构建、解析、操作的功能，并提供了对 ADT 数据类型的映射支持，很适合
于 Scala 3 的函数式编程风格。

1. wjson-core 提供了基本的 JSON 操作 API
   - JSON parer, 一个高性能的JSON解析器，支持标准的 JSON 和 JSON5 格式.
   - 基础的JSON API。
   - ADT 支持，支持 Case Class / enum 类型的 JSON mapping. 并利用 scala3 的 Macro 生成映射代码。
2. wjson-pattern 一个实验性的 JSON 模式匹配库，非常适合于从 JSON 中快速提取信息。
3. wjson-schema 一个 JSON schema 生成库，可以根据 ADT 类型生成 JSON schema.

## why another JSON library?
1. 更好的 ADT 支持， 相比其他的 JSON 库，几乎不用编写代码，就可以处理 Case class / enum 的 JSON 映射。
   (对比 spray-json，wjson 要简单太多)
2. 强大的 interpolation 支持，且可以在编译期完成 JSON 编译，避免运行时错误，并带来更好的运行性能。
3. JSON5 支持。JSON5 是 JSON 的超集，支持注释、多行字符串等特性，更适合于配置文件等场景。
4. 当然，wjson 也是我学习 Scala 3 的一个练手项目，尤其是对 Scala3 的 Macro 的使用。 wjson 广泛的使用 Macro 来提供一个简单、强大的 API。