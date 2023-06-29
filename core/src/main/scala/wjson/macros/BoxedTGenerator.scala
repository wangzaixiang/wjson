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
        '{ JsValueMapper.listMapping[t](using ${ref}) }.asExprOf[JsValueMapper[T]]

class SeqGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{ JsValueMapper.seqMapping[t](using ${ref}) }.asExprOf[JsValueMapper[T]]


class VectorGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{ JsValueMapper.vectorMapping[t](using ${ref}) }.asExprOf[JsValueMapper[T]]

class ArrayGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        val classTag: Expr[ClassTag[t]] = Expr.summon[ClassTag[t]].get // should exists
        '{ JsValueMapper.arrayMapping[t](using ${ref}, ${classTag}) }.asExprOf[JsValueMapper[T]]

class SetGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{ JsValueMapper.setMapping[t](using ${ref}) }.asExprOf[JsValueMapper[T]]

// type T = Option[X]
class OptionGenerator[T: Type] extends Generator[T]:
  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    baseTpe.asInstanceOf[AppliedType].args(0).asType match
      case '[t] =>
        val ref = summonJsValueMapper[t](deps).get
        '{ JsValueMapper.optionMapping[t](using ${ref}) }.asExprOf[JsValueMapper[T]]