package wjson

import wjson.JsValue.{JsArray, JsNumber}

import scala.annotation.experimental
import scala.collection.{IterableOps, SortedMap, SortedSet}
import scala.reflect.ClassTag
import wjson.macros.ADTMappingMacro

/**
 * type class for JsValue Mapping
 */
trait JsValueMapper[T]:
  def fromJson(js: JsValue): T
  def toJson(t: T): JsValue

extension [T: JsValueMapper](obj: T)
  inline def toJson: JsValue = summon[JsValueMapper[T]].toJson(obj)

extension (js: JsValue)
  inline def convertTo[T: JsValueMapper]: T = summon[JsValueMapper[T]].fromJson(js)

/**
 * T can implicitly convert to JsValue
 */
given[T: JsValueMapper]: Conversion[T, JsValue] with
  def apply(x: T): JsValue = summon[JsValueMapper[T]].toJson(x)

/**
 * support List,Seq,Vector,Set, SortedSet etc.
 */
given[T: JsValueMapper, CC[x] <: IterableOps[x, CC, CC[x]]]: Conversion[CC[T], JsArray] with
  inline def apply(x: CC[T]): JsArray = JsArray(x.map(x => summon[JsValueMapper[T]].toJson(x)).toList: _*)


private abstract class ADTMapping:
  inline def derived[T](using deriving.Mirror.Of[T]): JsValueMapper[T] = ${ ADTMappingMacro.genADTImpl[T] }
  inline given adtMapper[T](using deriving.Mirror.Of[T]): JsValueMapper[T] = ${ ADTMappingMacro.genADTImpl[T] }


private abstract class CollectionMapping extends ADTMapping:

  given[T: JsValueMapper : ClassTag]: JsValueMapper[Array[T]] = arrayMapping[T]
  def arrayMapping[T: JsValueMapper : ClassTag]: JsValueMapper[Array[T]] = new JsValueMapper[Array[T]]:
    inline def fromJson(js: JsValue): Array[T] = (js: @unchecked) match
      case x: JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)).toArray
      case _ => throw new Exception(s"Expected JsArray but ${js.getClass}")

    inline def toJson(t: Array[T]): JsValue = JsArray(t.map(x => summon[JsValueMapper[T]].toJson(x)).toSeq)


  given [T: JsValueMapper]: JsValueMapper[List[T]] = listMapping[T]
  def listMapping[T: JsValueMapper]: JsValueMapper[List[T]] = new JsValueMapper[List[T]]:
    inline def fromJson(js: JsValue): List[T] = (js: @unchecked) match
      case x: JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)).toList
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")

    inline def toJson(t: List[T]): JsValue = JsArray(t.map(x => summon[JsValueMapper[T]].toJson(x)): _*)

  given [T: JsValueMapper]: JsValueMapper[Seq[T]] = seqMapping[T]
  def seqMapping[T: JsValueMapper]: JsValueMapper[Seq[T]] = new JsValueMapper[Seq[T]]:
    inline def fromJson(js: JsValue): Seq[T] = (js: @unchecked) match
      case x: JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x))
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")

    inline def toJson(t: Seq[T]): JsValue = JsArray(t.map(x => summon[JsValueMapper[T]].toJson(x)): _*)

  given [T: JsValueMapper]: JsValueMapper[Vector[T]] = vectorMapping[T]
  def vectorMapping[T: JsValueMapper]: JsValueMapper[Vector[T]] = new JsValueMapper[Vector[T]]:
    inline def fromJson(js: JsValue): Vector[T] = (js: @unchecked) match
      case x: JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)).toVector
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")

    inline def toJson(t: Vector[T]): JsValue = JsArray(t.map(x => summon[JsValueMapper[T]].toJson(x)): _*)

  given [T: JsValueMapper]: JsValueMapper[Set[T]] = setMapping[T]
  def setMapping[T: JsValueMapper]: JsValueMapper[Set[T]] = new JsValueMapper[Set[T]]:
    inline def fromJson(js: JsValue): Set[T] = (js: @unchecked) match
      case x: JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)).toSet
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")

    inline def toJson(t: Set[T]): JsValue = JsArray(t.toSeq.map(x => summon[JsValueMapper[T]].toJson(x)): _*)

  given[T: JsValueMapper : Ordering]: JsValueMapper[SortedSet[T]] = sortedSetMapping[T]
  def sortedSetMapping[T: JsValueMapper : Ordering]: JsValueMapper[SortedSet[T]] = new JsValueMapper[SortedSet[T]]:
    inline def fromJson(js: JsValue): SortedSet[T] = (js: @unchecked) match
      case x: JsArray => SortedSet(x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)): _*)
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")

    inline def toJson(t: SortedSet[T]): JsValue = JsArray(t.toList.map(x => summon[JsValueMapper[T]].toJson(x)): _*)

  given[T: JsValueMapper]: JsValueMapper[Map[String, T]] = mapMapping[T]
  def mapMapping[T: JsValueMapper]: JsValueMapper[Map[String, T]] = new JsValueMapper[Map[String, T]]:
    inline def fromJson(js: JsValue): Map[String, T] = (js: @unchecked) match
      case o: JsObject => o.fields.map(x => (x._1, summon[JsValueMapper[T]].fromJson(x._2))).toMap
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")

    inline def toJson(t: Map[String, T]): JsValue = JsObject(t.toList.map(x => (x._1, summon[JsValueMapper[T]].toJson(x._2))))

  given[T: JsValueMapper]: JsValueMapper[SortedMap[String, T]] = sortedMapMapping[T]
  def sortedMapMapping[T: JsValueMapper]: JsValueMapper[SortedMap[String, T]] = new JsValueMapper[SortedMap[String, T]]:
    inline def fromJson(js: JsValue): SortedMap[String, T] = (js: @unchecked) match
      case o: JsObject => SortedMap(o.fields.map(x => (x._1, summon[JsValueMapper[T]].fromJson(x._2))).toSeq: _*)
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")

    inline def toJson(t: SortedMap[String, T]): JsValue = JsObject(t.toList.map(x => (x._1, summon[JsValueMapper[T]].toJson(x._2))))

private abstract class OptionMapping extends CollectionMapping:

  def optionMapping[T: JsValueMapper]: JsValueMapper[Option[T]] = new JsValueMapper[Option[T]]:
    inline def fromJson(js: JsValue): Option[T] = (js: @unchecked) match
      case JsNull => None
      case _ => Some(summon[JsValueMapper[T]].fromJson(js))

    inline def toJson(t: Option[T]): JsValue = (t: @unchecked) match
      case x: Some[T] => summon[JsValueMapper[T]].toJson(x.value)
      case None => JsNull

  given[T: JsValueMapper]: JsValueMapper[Option[T]] = optionMapping[T]


private abstract class PrimitiveMapping extends OptionMapping:

  given JsValueMapper[Boolean] with
    inline def fromJson(js: JsValue): Boolean = (js: @unchecked) match
      case x: JsBoolean => x.value
      case _ => throw new Exception(s"Expected JsBoolen but ${js.getClass}")

    inline def toJson(t: Boolean): JsValue = if (t) JsValue.JsTrue else JsValue.JsFalse

  given JsValueMapper[Byte] with
    inline def fromJson(js: JsValue): Byte = (js: @unchecked) match
      case JsNumber(num: Long) => num.toByte
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Byte): JsValue = JsNumber(t.toLong)

  given JsValueMapper[Short] with
    inline def fromJson(js: JsValue): Short = (js: @unchecked) match
      case JsNumber(num: Long) => num.toShort
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Short): JsValue = JsNumber(t.toLong)

  given JsValueMapper[Int] with
    inline def fromJson(js: JsValue): Int = (js: @unchecked) match
      case JsNumber(num: Long) => num.toInt
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Int): JsValue = JsNumber(t.toLong)

  given JsValueMapper[Long] with
    inline def fromJson(js: JsValue): Long = (js: @unchecked) match
      case JsNumber(num: Long) => num
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Long): JsValue = JsNumber(t)

  given JsValueMapper[Float] with
    inline def fromJson(js: JsValue): Float = (js: @unchecked) match
      case JsNumber(num: Double) => num.toFloat
      case JsNumber(num: Long) => num.toFloat
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Float): JsValue = JsNumber(t.toDouble)

  given JsValueMapper[Double] with
    inline def fromJson(js: JsValue): Double = (js: @unchecked) match
      case JsNumber(num: Double) => num
      case JsNumber(num: Long) => num.toDouble
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: Double): JsValue = JsNumber(t)

  given JsValueMapper[BigDecimal] with
    inline def fromJson(js: JsValue): BigDecimal = (js: @unchecked) match
      case JsNumber(num: Double) => BigDecimal(num)
      case JsNumber(num: Long) => BigDecimal(num)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[BigInt] with
    inline def fromJson(js: JsValue): BigInt = (js: @unchecked) match
      case JsNumber(x: Long) => BigInt(x)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: BigInt): JsValue = JsNumber(t.doubleValue)

  given jmBigDecimal: JsValueMapper[java.math.BigDecimal] with
    inline def fromJson(js: JsValue): java.math.BigDecimal = (js: @unchecked) match
      case JsNumber(num: Double) => java.math.BigDecimal(num)
      case JsNumber(num: Long) => java.math.BigDecimal(num)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: java.math.BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[java.math.BigInteger] with
    inline def fromJson(js: JsValue): java.math.BigInteger = (js: @unchecked) match
      case JsNumber(x: Long) => java.math.BigInteger(x.toString)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")

    inline def toJson(t: java.math.BigInteger): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[String] with
    inline def fromJson(js: JsValue): String = (js: @unchecked) match
      case x: JsString => x.value
      case _ => throw new Exception(s"Expected JsString but ${js.getClass}")

    inline def toJson(t: String): JsValue = JsString(t)

private abstract class NativeMapping extends PrimitiveMapping:

  given JsValueMapper[JsValue] with
    inline def fromJson(js: JsValue): JsValue = js

    inline def toJson(t: JsValue): JsValue = t

  given JsValueMapper[JsBoolean] with
    inline def fromJson(js: JsValue): JsBoolean = js match
      case x: JsBoolean => x
      case _ => throw new Exception(s"expect JsBoolean but ${js.getClass}")

    inline def toJson(t: JsBoolean): JsValue = t

  given JsValueMapper[JsString] with
    inline def fromJson(js: JsValue): JsString = (js: @unchecked) match
      case x: JsString => x
      case _ => throw new Exception(s"expect JsString but ${js.getClass}")

    inline def toJson(t: JsString): JsValue = t

  given JsValueMapper[JsNumber] with
    inline def fromJson(js: JsValue): JsNumber = (js: @unchecked) match
      case x: JsNumber => x
      case _ => throw new Exception(s"expect JsNumber but ${js.getClass}")

    inline def toJson(t: JsNumber): JsValue = t

  given JsValueMapper[JsObject] with
    inline def fromJson(js: JsValue): JsObject = (js: @unchecked) match
      case x: JsObject => x
      case _ => throw new Exception(s"expect JsObject but ${js.getClass}")

    inline def toJson(t: JsObject): JsValue = t

  given JsValueMapper[JsArray] with
    inline def fromJson(js: JsValue): JsArray = (js: @unchecked) match
      case x: JsArray => x
      case _ => throw new Exception(s"expect JsArray but ${js.getClass}")

    inline def toJson(t: JsArray): JsValue = t

object JsValueMapper extends NativeMapping
