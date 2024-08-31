# core
wJson 是一个Scala3 JSON库，它提供了一个简单的、直观的API，用于在Scala中处理JSON。

wJson的设计参考了如下的库，并试图变得提供更为 简单的API：
1. [Spray JSON](github.com/spray-json/spray-json)
2. [wangzaixiang's fork of Spray JSON](github.com/wangzaixiang/spray-json)
3. [ujson](github.com/lihaoyi/ujson)

wJson 基于scala3，也是我学习Scala3的一个实践项目，通过 wJson，全面熟悉一下 scala3 的语法，以及新的 Macro 系统的使用。

## 安装

以 sbt 为例：
```
libraryDependencies += "com.github.wangzaixiang" %% "wjson" % "0.1.0"
```

## 使用

在使用 wjson 库之前，简单引入一下：
```scala
import wjson.{*, given}
```
