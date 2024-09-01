# core
wjson 是一个Scala3 JSON库，它提供了一个简单的、直观的API，用于在Scala中处理JSON。

wJson的设计参考了如下的库，并试图变得提供更为 简单的API：
1. [Spray JSON](github.com/spray-json/spray-json)
2. [wangzaixiang's fork of Spray JSON](github.com/wangzaixiang/spray-json)
3. [ujson](github.com/lihaoyi/ujson)

wjson-core 是 wjson 的核心模块，它提供了基本的 JSON 的核心数据结构，以及基本的 JSON 解析和序列化功能。
- JsValue API： 对 JSON 数据结构的 ADT 抽象，支持 immutable 风格的 build, access, show 等操作，是 wjson 的核心数据结构。
- JSON parer, 一个高性能的JSON解析器，支持标准的 JSON 语法
- JSON5 parser, JSON5 语法解析器。[JSON5](https://json5.org) 是 JSON 的超集，支持注释、多行字符串等特性。
- json"..." 和 json5"..." 字符串插值
- ADT 支持

## 安装

以 sbt 为例：
```
libraryDependencies += "com.github.wangzaixiang" %% "wjson-core" % "0.5.0-RC1"
```

## 使用

在使用 wjson 库之前，简单引入一下：
```scala
import wjson.{*, given}
```
