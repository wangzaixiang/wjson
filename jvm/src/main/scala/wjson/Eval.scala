package wjson

import org.mvel2.MVEL
import scala.jdk.CollectionConverters.*

object Eval {

  def eval(script: String, context: Map[String, Any]): Any =
     MVEL.eval(script, context.asJava)

}
