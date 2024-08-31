## 多版本支持

```scala 3
@js.version("1.0.6") case class Cube
(
   @js.since("1.0.5")
   name: Option[String],      // 这个元素是 1.0.5 版本开始引入的

   @js.deprecated("1.0.6")    // 在 1.0.6 这个版本中被废弃
   age: Option[Int],

   column: Column,    // 是一个枚举类型， 新版本，可以增加新的 枚举值
   dimension: Dimension // 是一个 Product 类型， 新版本，可以增加新的字段
)

```

场景：
1. 新版本中，对某个 Product 类型增加新的字段
    - 该字段需要声明为 Option 类型，以兼容旧版本
    - 通过 @js.since("1.0.5") 来标记该字段是从 1.0.5 版本开始引入的
2. 新版本中，对某个枚举类型增加新的枚举值
3. 读取更新版本的数据时，由于当前版本中没有该字段（或枚举值），会自动的忽略该字段（或枚举值）
4. 读取旧版本的数据时，会自动的填充默认值（对于 Option 类型，填充为 None）