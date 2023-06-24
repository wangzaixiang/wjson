package wjson.macros

import ADTMappingMacro.*
import wjson.*
import scala.quoted.*
import scala.reflect.ClassTag


// TODO, add more collection here
class ListGenerator[T: Type] extends Generator[T]:

  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{
          new JsValueMapper[List[t]]:
            def fromJson(js: JsValue): List[t] = js match
              case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toList
              case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")

            def toJson(x: List[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
        }.asExprOf[JsValueMapper[T]]

class SeqGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{
          new JsValueMapper[Seq[t]]:
            def fromJson(js: JsValue): Seq[t] = js match
              case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toSeq
              case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")

            def toJson(x: Seq[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
        }.asExprOf[JsValueMapper[T]]

class VectorGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{
          new JsValueMapper[Vector[t]]:
            def fromJson(js: JsValue): Vector[t] = js match
              case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toVector
              case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")

            def toJson(x: Vector[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
        }.asExprOf[JsValueMapper[T]]

class ArrayGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        val classTag: Expr[ClassTag[t]] = Expr.summon[ClassTag[t]].get // should exists
        '{
          new JsValueMapper[Array[t]]:
            def fromJson(js: JsValue): Array[t] = js match
              case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toArray(using ${ classTag })
              case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")

            def toJson(x: Array[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
        }.asExprOf[JsValueMapper[T]]

class SetGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{
          new JsValueMapper[Set[t]]:
            def fromJson(js: JsValue): Set[t] = js match
              case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toSet
              case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")

            def toJson(x: Set[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)).toSeq)
        }.asExprOf[JsValueMapper[T]]

//  class SortedSetGenerator[T: Type] extends Generator[T]:
//    override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
//      import quotes.reflect.*
//      baseTpe.asInstanceOf[AppliedType].args(0).asType match
//        case '[t] =>
//          val ref = summonJsValueMapper[t](deps).get
//          val ordering: Expr[Ordering[t]] = Expr.summon[Ordering[t]].get // should exists
//          '{
//            new JsValueMapper[SortedSet[t]]:
//              def fromJson(js: JsValue): SortedSet[t] = js match
//                case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toSet
//                case _ => throw new IllegalArgumentException(s"expect JsArray, but got: $js")
//
//              def toJson(x: Set[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
//          }.asExprOf[JsValueMapper[T]]
