# wjson Pattern: 一个简单、直观的Scala JSON库

JSON作为数据交换标准，使用越来越广泛。对输入的 JSON 进行模式匹配、信息提取，并进行进一步的加工处理，是一个非常常见的场景。

应用场景：

Mock Server。在Mock Server中，我们需要配置大量的 Mock 规则，在 request 匹配规则的情况下，返回相应的数据。
当request/response 都可以使用JSON来表达的时候，我们就可以使用 JSON Pattern 来简单的描述一个 Mock Rule。

流量回放改写规则。在对录制流量进行回放时，我们需要配置一系列的改写规则，匹配某些条件的请求和返回的关键字段，被提取出来，
并用户改写后续请求的参数，以满足回放的需求。 

使用脚本来进行JSON数据的处理。

测试工具。测试工具非常需要一个简单、高效的结果校验工具，对返回数据是JSON的场景，如果我们需要使用大量的字段提取、字段校验的话，
是非常枯燥的。而使用一个模式匹配工具，可以简化这一过程。

wjson 自定义了一个 JSON Pattern 语言，这个语言参考了：[rejson](github.com/squaremo/rejson) 的设计，但进行了增强。
我们尽力保证这个语言的语法一致性，以让使用者尽可能快速的熟悉其使用方式。但客观来说，定义一个新的DSL，
并不是一件容器的事情，尤其是其对于使用者而言，是否有较低的认知成本（学习成本），还是存在很大的未知的。
(除非必要，不要轻易的创建DSL)，因此，wjson-pattern 目前是一个实验性的库，我们会根据使用者的反馈，不断的改进。

1. 简单示例

先看一个简单的 wjson pattern 示例：

```json5
// JSON data: curl https://api.github.com/repos/wangzaixiang/wjson/commits?per_page=1
{
"sha": "650e56cd380c311909cd50408bbb4884f1f5d21e",
"node_id": "C_kwDOHj94ltoAKDY1MGU1NmNkMzgwYzMxMTkwOWNkNTA0MDhiYmI0ODg0ZjFmNWQyMWU",
"commit": {
  "author": {
    "name": "wangzaixiang",
    "email": "949631531@qq.com",
    "date": "2022-07-05T14:23:09Z"
  },
  "committer": {
    "name": "wangzaixiang",
    "email": "949631531@qq.com",
    "date": "2022-07-05T14:23:09Z"
  },
  "message": "add taged string pattern support\nadd array index/filter support",
  "tree": {
    "sha": "89fbbf82e3c5ffd0e1a7978dd7778195a004df2c",
    "url": "https://api.github.com/repos/wangzaixiang/wjson/git/trees/89fbbf82e3c5ffd0e1a7978dd7778195a004df2c"
  },
  "url": "https://api.github.com/repos/wangzaixiang/wjson/git/commits/650e56cd380c311909cd50408bbb4884f1f5d21e",
  "comment_count": 0,
  "verification": {
    "verified": false,
    "reason": "unsigned",
    "signature": null,
    "payload": null
  }
},
"url": "https://api.github.com/repos/wangzaixiang/wjson/commits/650e56cd380c311909cd50408bbb4884f1f5d21e",
"html_url": "https://github.com/wangzaixiang/wjson/commit/650e56cd380c311909cd50408bbb4884f1f5d21e",
"comments_url": "https://api.github.com/repos/wangzaixiang/wjson/commits/650e56cd380c311909cd50408bbb4884f1f5d21e/comments",
"parents": [
  {
    "sha": "d90609ac4e7254eac5138453e2e07591a11bb55e",
    "url": "https://api.github.com/repos/wangzaixiang/wjson/commits/d90609ac4e7254eac5138453e2e07591a11bb55e",
    "html_url": "https://github.com/wangzaixiang/wjson/commit/d90609ac4e7254eac5138453e2e07591a11bb55e"
  }
]
}
```

```jsonpattern
// JSON Pattern
{
  sha: @sha,
  commit: { author: { name: @commit_name } }
  url: @url,
  parents/*/sha: @parents
}
```
对上述的示例 JSON，我们可以使用如下的 JSON pattern 来对其进行匹配，并完成相应字段的提取：

```scala 3
val info = "...json string..." 
  info.parseJson match
      case rejson"""
        {
          sha: $sha@_,
          commit: { author: { name: $commit_name@_ } },
          url: $url@_,
          parents/*/sha: $parents@_
        }
      """ =>
        println(s"sha = $sha, commit_name = $commit_name, url = $url, parents=$parents")
      
      case _ => println("not matched"

//
// sha = JsString(650e56cd380c311909cd50408bbb4884f1f5d21e), 
// commit_name = JsString(wangzaixiang), 
// url = JsString(https://api.github.com/repos/wangzaixiang/wjson/commits/650e56cd380c311909cd50408bbb4884f1f5d21e), 
// parents=JsArray(List(JsString(d90609ac4e7254eac5138453e2e07591a11bb55e)))
```

## wjson pattern 语法说明

wjson pattern 是 JSON 语法的一个扩展：

1. 在 # 之后的内容，是注释，不会影响匹配
2. 支持基本类型：null, boolean(true, false), number, string（'hello' or "hello"）
   ```jsonpattern
        [ 1, 2.0, true, false, "hello", null ]
        'hello' # single quote string
    ```
3. 支持复杂类型： array: `[ ... ]`, object `{ name: value }`。 在 object 中可以使用 类似于 JSON5 的语法，以增强可读性
    ```jsonpattern
      [ 1, 2, 3]                    # array
      { "name": "John", "age": 20 }
      { 'name': 'John', 'age': 20 }  # 与上一行等效
      { name: 'John', age: 20 }  # 与上一行等效
    ```
4. 在所有可以是值的地方，可以使用 name@value 的语法，将value 的值绑定到name变量中返回。
   ```jsonpattern
    a @ 1  # 匹配 1 且将值绑定到 a 变量
    b @ [ 1,2,3 ] # 匹配 [1,2,3] 且将值绑定到 b 变量
    c @ { name: "John", age: 20 }  # 匹配 { name: "John", age: 20 } 且将值绑定到 c 变量
    [ 1, 2, x @ 3, 4 ]     # 匹配 [1,2,3,4] 且将 3绑定到 x 变量
    { name: n@string, age: a@number }  # 匹配 { name: "John", age: 20 } 且将 "John" 绑定到 n 变量，20 绑定到 a 变量
   ```
5. 类型匹配： "boolean", "string", "number", "integer", "array", "object" 匹配对应的JS类型
    ```jsonpattern
    # input: { name: "John", age: 20, leader: true }
    { name: name @ string, age: age @ number, leader: leader @ boolean }
    # 匹配成功，并将 name 绑定到 "John", age 绑定到 20, leader 绑定到 true
    ```
6. "_" 匹配任意单值
    ```jsonpattern 
    [ 1, 2, _, 4]  #  match: [ 1,2,3,4], not match: [1,2,3,5,4]
    { name: _, age: 20 } # match { name: "John", age: 20 } but not { age: 20 }
    ```

7. "_*" 在数组中，可以匹配 0 到多个数组成员，在对象中，可以匹配未被指定的所有其他字段。
    ```jsonpattern 
    { name: "John", other@_* }
    # match { name: "Johe", age:20 , leader:true} and others := { age: 20, leader: true}
    [ 1,2, o@_*, 10 ] 
    # match [ 1,2,3,4,5,6,7,8,9,10] and o:=[3,4,5,6,7,8,9]
    ```

8. 可以使用 tag"content"的方式扩展匹配规则，这个规则可以有用户来使用自定义的方式对值进行匹配。wjson中提供了 eval"expr" 和 r"expr" 两个参考实现，mvel使用MVEL表达式引擎来匹配一个JS值，r使用正则表达式来匹配值。
    ```jsonpattern
    { name: "John", age: eval"it >= 0 && it <= 100", email: r"\w+@(\w\.)*\w" }
    # match { name: "John", age: 20,  email: "john@qq.com" }
    ```
9. 可以使用 a/b/c 的路径来匹配，等效于 { a : { b: {c : _ } } } 。
    ```jsonpattern 
    { a/b/c: "John" } 
   # match { a: { b:  {c : "John" } } }
    ```

10. 可以使用 `/*` 来匹配数组中的所有成员，等效于 `[ _ ]` 。
    ```jsonpattern
    { users/*/name: "John" }
    ```
    
11. 可以使用 `a[ pattern ]` 或者 `a[n]` 的方式来对数组中的成员进行条件筛选(或选中第n个成员)。
     ```jsonpattern
     # { users: [ { name: "John", age: 20, email: "john@qq.com" },
     #            { name: "rose“, ageL 25, email: "rose@qq.com" } ]  }
     { users[ {name:"John"} ]/email: @email } 
     # 匹配成功，并绑定 变量 email 的值为 "john@qq.com"
    ```
