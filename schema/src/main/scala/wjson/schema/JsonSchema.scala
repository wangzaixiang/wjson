package wjson.schema

import wjson.*

import scala.annotation.StaticAnnotation
import scala.quoted.Type

object JsonSchema:

  enum JsonFormat:
    case Date, Time, DateTime, Duration
    case Email
    case Hostname
    case Ipv4, IpV6
    case Uri
    case UUID

  /**
   * annot a type, a field's description
   */
  class description(val value: String) extends StaticAnnotation

  /**
   * annot a type's value constraint
   */
  class enums(val values: List[String]|List[Int]|List[Double]) extends StaticAnnotation

  class multipleOf(val value: Int) extends StaticAnnotation
  class maximum(val value: Int|Double) extends StaticAnnotation
  class minimum(val value: Int|Double) extends StaticAnnotation
  class exclusiveMaximum(val value: Int|Double) extends StaticAnnotation
  class exclusiveMinimum(val value: Int|Double) extends StaticAnnotation

  class maxLength(val value: Int) extends StaticAnnotation
  class minLength(val value: Int) extends StaticAnnotation
  class pattern(val value: String) extends StaticAnnotation

  class maxItems(val value: Int) extends StaticAnnotation
  class minItems(val value: Int) extends StaticAnnotation
  class uniqueItems(val value: Boolean) extends  StaticAnnotation
  class maxContains(val value: Int|Double) extends StaticAnnotation
  class minContains(val value: Int|Double) extends StaticAnnotation

  class maxProperties(val value: Int) extends StaticAnnotation
  class minProperties(val value: Int) extends StaticAnnotation
  class format(val value: String|JsonFormat) extends StaticAnnotation

  /**
   * mark a field as open field, and the type's `additionalProperties` as true
   */
  class open extends StaticAnnotation

  /**
   * mark a field as dynamic, and it's JSON will having a `_$type` field
   */
  class dynamic extends StaticAnnotation

  /**
   * mark the field is an inside-document pointer
   */
  case class Pointer[T](value: String)

  /**
   * mark the field is an outside-document pointer
   */
  case class GlobalPointer[T](value: String)

  given [T]: Conversion[String, Pointer[T]] with
    override def apply(x: String): Pointer[T] = Pointer(x)

  given [T]: JsValueMapper[Pointer[T]] with
    override def toJson(x: Pointer[T]): JsValue = JsString(x.value)
    override def fromJson(x: JsValue): Pointer[T] = x match
      case JsString(value) => Pointer(value)
      case _ => throw IllegalArgumentException(s"expect JsString, but got ${x}")


  /**
   * return a Json Schema of type T
   */
  def of[T: Type]: String = ???