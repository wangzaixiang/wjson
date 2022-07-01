package wjson

import scala.collection.{IterableOps, SortedMap, SortedSet, mutable}
import scala.reflect.ClassTag

/**
 * Json Model ADT
 */
enum JsValue:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double)
    case JsString(value: String)
    case JsArray(elements: Seq[JsValue])
    case JsObject(fields: Map[String, JsValue])


object JsValue:
  val JsTrue: JsBoolean = JsBoolean(true)
  val JsFalse: JsBoolean = JsBoolean(false)
  val JsZero: JsNumber = JsNumber(0)
  val JsEmptyString: JsString = JsString("")
  val JsEmptyObject: JsObject = JsObject()
  val JsEmptyArray: JsArray = JsArray()

  def parse(str: String): JsValue = JsonParser.parse(ParserInput(str))
  def JsObject(fields: (String, JsValue)*): JsObject = JsObject(Map(fields: _*))
  def JsArray(elements: JsValue*): JsArray = JsArray(elements.toList)

  /**
   * T can implicitly convert to JsValue
   */
  given [T: JsValueMapper]: Conversion[T, JsValue] with
    def apply(x: T): JsValue = summon[JsValueMapper[T]].toJson(x)

  extension (value: JsValue)
    def show(indent: Int = 2, margin: Int = 100): String =
      val buffer = new StringBuilder

      def show0(value: JsValue, indentString: String): Unit =
        value match
          case JsNull => buffer.append("null")
          case JsBoolean(value) => buffer.append(value)
          case JsNumber(value) => buffer.append(value)
          case JsString(value) => buffer.append(escapedString(value))
          case JsObject(fields) =>
            buffer.append("{\n")
            fields.foreach { case (name, value) =>
              buffer.append(indentString + " " * indent)
              buffer.append(escapedString(name))
              buffer.append(":")
              show0(value, indentString + " " * indent)
              buffer.append(",\n")
            }
            buffer.append(indentString)
            buffer.append("}")
          case JsArray(elements) =>
            buffer.append("[\n")
            elements.foreach { elem =>
              buffer.append( indentString + " " * indent)
              show0(elem, indentString + " " * indent)
              buffer.append(",\n")
            }
            buffer.append(indentString).append("]")

      // TODO unicode processing
      def escapedString(str: String): String = "\"" + str.replace("\"", "\\\"") + "\""
      show0(value, "")
      buffer.toString

  /**
   * support List,Seq,Vector,Set, SortedSet etc.
   */
  given [T: JsValueMapper, CC[x] <: IterableOps[x, CC, CC[x]]]: Conversion[CC[T], JsArray] with
    def apply(x: CC[T]): JsArray = JsArray( x.map(summon[JsValueMapper[T]].toJson).toList:_* )

export JsValue.{JsNull, JsBoolean, JsNumber, JsString, JsArray, JsObject}

extension (str:String)
  def parseJson: JsValue = JsValue.parse(str)

/**
 * type class for JsValue Mapping
 */
trait JsValueMapper[T]:
  def fromJson(js: JsValue): T
  def toJson(t: T): JsValue

/**
 *
 * Best Practice for Case Class Json Mapping:
 *
 * 1. make the case class `derives JsValueMapper`.
 * 2. or declare a given JsValueMaper[T] for the case class
 *
 * otherwise, every time you use API which using a JsValueMapper[T], a new JsValueMapper[T] will be
 * created via the JsValueMapper.given_JsValueMapper_T macro. this is an `expensive` operation, every
 * macro expansion will generate an anonumous implementation class. so, there will be a lot of classes
 * in the compilation and increase the compile time, the output jar file will be large.
 */
object JsValueMapper:
  inline def derived[T](using deriving.Mirror.ProductOf[T]): JsValueMapper[T] =
    ${ JsValueMapperMacro.generateImpl[T] }

  /**
   * the T.derived has high priority than JsValMapper.given so if you define the derives,
   * this macro will not be used.
   */
  inline given[T](using deriving.Mirror.ProductOf[T]): JsValueMapper[T] =
    ${JsValueMapperMacro.generateImpl[T]}

  given JsValueMapper[Boolean] with
    def fromJson(js: JsValue): Boolean = js match
      case JsBoolean(value) => value
      case _ => throw new Exception(s"Expected JsBoolen but ${js.getClass}")
    def toJson(t: Boolean): JsValue = if(t) JsValue.JsTrue else JsValue.JsFalse

  given JsValueMapper[Byte] with
    def fromJson(js: JsValue): Byte = js match
      case JsNumber(value) => value.toByte
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Byte): JsValue = JsNumber(t)

  given JsValueMapper[Short] with
    def fromJson(js: JsValue): Short = js match
      case JsNumber(value) => value.toShort
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Short): JsValue = JsNumber(t)

  given JsValueMapper[Int] with
    def fromJson(js: JsValue): Int = js match
      case JsNumber(value) => value.toInt
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Int): JsValue = JsNumber(t)

  given JsValueMapper[Long] with
    def fromJson(js: JsValue): Long = js match
      case JsNumber(value) => value.toLong
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Long): JsValue = JsNumber(t)

  given JsValueMapper[Float] with
    def fromJson(js: JsValue): Float = js match
      case JsNumber(value) => value.toFloat
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Float): JsValue = JsNumber(t)

  given JsValueMapper[Double] with
    def fromJson(js: JsValue): Double = js match
      case JsNumber(value) => value
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: Double): JsValue = JsNumber(t)

  given JsValueMapper[BigDecimal] with
    def fromJson(js: JsValue): BigDecimal = js match
      case JsNumber(value) => BigDecimal(value)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[BigInt] with
    def fromJson(js: JsValue): BigInt = js match
      case JsNumber(value) => BigInt(value.toString)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t:BigInt): JsValue = JsNumber(t.doubleValue)

  given jmBigDecimal: JsValueMapper[java.math.BigDecimal] with
    def fromJson(js: JsValue): java.math.BigDecimal = js match
      case JsNumber(value) => java.math.BigDecimal(value)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t: java.math.BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[java.math.BigInteger] with
    def fromJson(js: JsValue): java.math.BigInteger = js match
      case JsNumber(value) => java.math.BigInteger(value.toString)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    def toJson(t:java.math.BigInteger): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[String] with
    def fromJson(js: JsValue): String = js match
      case JsString(value) => value
      case _ => throw new Exception(s"Expected JsString but ${js.getClass}")
    def toJson(t: String): JsValue = JsString(t)

  // TODO is there a better way to do this? Seq/List/Vector/Set/SortedSet/Map/SortedMap
  given [T:JsValueMapper :ClassTag]: JsValueMapper[Array[T]] with
    def fromJson(js: JsValue): Array[T] = js match
      case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson).toArray
      case _ => throw new Exception(s"Expected JsArray but ${js.getClass}")
    def toJson(t: Array[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson))


  given [T:JsValueMapper]: JsValueMapper[Seq[T]] with
    def fromJson(js: JsValue): Seq[T] = js match
        case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson)
        case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    def toJson(t: Seq[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson) )

  given [T: JsValueMapper]: JsValueMapper[List[T]] with
    def fromJson(js: JsValue): List[T] = js match
      case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson).toList
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    def toJson(t: List[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

  given [T: JsValueMapper]: JsValueMapper[Vector[T]] with
    def fromJson(js: JsValue): Vector[T] = js match
      case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson).toVector
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    def toJson(t: Vector[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

  given [T: JsValueMapper]: JsValueMapper[Set[T]] with
    def fromJson(js: JsValue): Set[T] = js match
      case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson).toSet
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    def toJson(t: Set[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson).toSeq:_*)

  given [T: JsValueMapper :Ordering]: JsValueMapper[SortedSet[T]] with
    def fromJson(js: JsValue): SortedSet[T] = js match
      case JsArray(value) => SortedSet( value.map(summon[JsValueMapper[T]].fromJson):_* )
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    def toJson(t: SortedSet[T]): JsValue = JsArray(t.toList.map(summon[JsValueMapper[T]].toJson):_*)

  given [T:JsValueMapper]: JsValueMapper[Map[String, T]] with
    def fromJson(js: JsValue): Map[String, T] = js match
      case JsObject(value) => value.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toMap
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    def toJson(t: Map[String, T]): JsValue = JsObject( t.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T:JsValueMapper]: JsValueMapper[SortedMap[String, T]] with
    def fromJson(js: JsValue): SortedMap[String, T] = js match
      case JsObject(fields) => SortedMap( fields.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toSeq:_* )
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    def toJson(t: SortedMap[String, T]): JsValue = JsObject( t.toMap.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T: JsValueMapper]: JsValueMapper[Option[T]] with
    def fromJson(js: JsValue): Option[T] = js match
      case JsNull => None
      case _ => Some(summon[JsValueMapper[T]].fromJson(js))
    def toJson(t: Option[T]): JsValue = t match
      case Some(x) => summon[JsValueMapper[T]].toJson(x)
      case None => JsNull

  def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fields.get(name) match
      case Some(JsNull) => throw new Exception("Expected field " + name + " not exists in JSON")
      case Some(value) => summon[JsValueMapper[T]].fromJson(value)
      case None => throw new Exception("Expected field " + name + " not exists in JSON")

  def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default:T): T =
    js.fields.get(name) match
      case Some(JsNull) => default
      case Some(value) => summon[JsValueMapper[T]].fromJson(value)
      case None => default

extension [T: JsValueMapper](obj: T)
  def toJson: JsValue = summon[JsValueMapper[T]].toJson(obj)

extension (js: JsValue)
  def convertTo[T: JsValueMapper]: T = summon[JsValueMapper[T]].fromJson(js)

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def rejson = new RejsonInterpolation(sc)

