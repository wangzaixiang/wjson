//package wjson
//
//import lexer.RejsonLexer
//import rejson.parser.Program
//
//object Demo {
//
//  def main(args: Array[String]): Unit = {
//    val source =
//      """
//      { name: "John",
//        age: number,
//        a/b/c : "hello",
//        data/order: {
//          skuId: mySkuId@_
//        },
//        nums: range"1,100",
//        user: mod"2n+1",
//        ids: (1,2,3,4),
//        _
//      }
//      """
//
//    val resp =
//      """
//      {  order: { skuId: ${mySkuId}, skuId: ${req.data.order.skuId} }
//      }
//      """
//
//    val lexer = new RejsonLexer(source)
//    val parser = new Program(lexer)
//    val succ  = parser.parse()
//
//    println(s"compiled $succ")
//
//    println(parser.root)
//  }
//
//}
