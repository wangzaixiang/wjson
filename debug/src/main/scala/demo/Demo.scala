package demo

import java.util
import scala.collection.SortedSet

object Demo:

  def main(args: Array[String]): Unit =
    val o1 = new Object
    val o2 = new Object
    val x: Ordering[Object] = new Ordering[Object]:
      override def compare(x: Object, y: Object): Int = 0
    val oo = SortedSet.apply(o1, o2)(using x)
    println(oo)
