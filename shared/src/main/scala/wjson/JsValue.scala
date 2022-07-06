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

  given JsValueMapper[JsBoolean] with
    inline def fromJson(js: JsValue): JsBoolean = js match
      case x: JsBoolean => x
      case _ => throw new Exception(s"expect JsBoolean but ${js.getClass}")
    inline def toJson(t: JsBoolean): JsValue = t

  given JsValueMapper[JsString] with
    inline def fromJson(js: JsValue): JsString = js match
      case x: JsString => x
      case _ => throw new Exception(s"expect JsString but ${js.getClass}")
    inline def toJson(t: JsString): JsValue = t

  given JsValueMapper[JsNumber] with
    inline def fromJson(js: JsValue): JsNumber = js match
      case x: JsNumber => x
      case _ => throw new Exception(s"expect JsNumber but ${js.getClass}")
    inline def toJson(t: JsNumber): JsValue = t

  given JsValueMapper[JsObject] with
    inline def fromJson(js: JsValue): JsObject = js match
      case x: JsObject => x
      case _ => throw new Exception(s"expect JsObject but ${js.getClass}")
    inline def toJson(t: JsObject): JsValue = t

  given JsValueMapper[JsArray] with
    inline def fromJson(js: JsValue): JsArray = js match
      case x: JsArray => x
      case _ => throw new Exception(s"expect JsArray but ${js.getClass}")
    inline def toJson(t: JsArray): JsValue = t

  given JsValueMapper[Boolean] with
    inline def fromJson(js: JsValue): Boolean = js match
      case x: JsBoolean => x.value
      case _ => throw new Exception(s"Expected JsBoolen but ${js.getClass}")
    inline def toJson(t: Boolean): JsValue = if(t) JsValue.JsTrue else JsValue.JsFalse

  given JsValueMapper[Byte] with
    inline def fromJson(js: JsValue): Byte = js match
      case x:JsNumber => x.value.toByte
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Byte): JsValue = JsNumber(t)

  given JsValueMapper[Short] with
    inline def fromJson(js: JsValue): Short = js match
      case x:JsNumber => x.value.toShort
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Short): JsValue = JsNumber(t)

  given JsValueMapper[Int] with
    inline def fromJson(js: JsValue): Int = js match
      case x:JsNumber => x.value.toInt
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Int): JsValue = JsNumber(t)

  given JsValueMapper[Long] with
    inline def fromJson(js: JsValue): Long = js match
      case x:JsNumber => x.value.toLong
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Long): JsValue = JsNumber(t.toDouble)

  given JsValueMapper[Float] with
    inline def fromJson(js: JsValue): Float = js match
      case x:JsNumber => x.value.toFloat
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Float): JsValue = JsNumber(t)

  given JsValueMapper[Double] with
    inline def fromJson(js: JsValue): Double = js match
      case x:JsNumber => x.value
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: Double): JsValue = JsNumber(t)

  given JsValueMapper[BigDecimal] with
    inline def fromJson(js: JsValue): BigDecimal = js match
      case x:JsNumber => BigDecimal(x.value)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[BigInt] with
    inline def fromJson(js: JsValue): BigInt = js match
      case x:JsNumber => BigInt(x.value.toString)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t:BigInt): JsValue = JsNumber(t.doubleValue)

  given jmBigDecimal: JsValueMapper[java.math.BigDecimal] with
    inline def fromJson(js: JsValue): java.math.BigDecimal = js match
      case x: JsNumber => java.math.BigDecimal(x.value)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t: java.math.BigDecimal): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[java.math.BigInteger] with
    inline def fromJson(js: JsValue): java.math.BigInteger = js match
      case x:JsNumber => java.math.BigInteger(x.value.toString)
      case _ => throw new Exception(s"Expected JsNumber but ${js.getClass}")
    inline def toJson(t:java.math.BigInteger): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[String] with
    inline def fromJson(js: JsValue): String = js match
      case x:JsString => x.value
      case _ => throw new Exception(s"Expected JsString but ${js.getClass}")
    inline def toJson(t: String): JsValue = JsString(t)

  // TODO is there a better way to do this? Seq/List/Vector/Set/SortedSet/Map/SortedMap
  given [T:JsValueMapper :ClassTag]: JsValueMapper[Array[T]] with
    inline def fromJson(js: JsValue): Array[T] = js match
      case x:JsArray => x.elements.map(summon[JsValueMapper[T]].fromJson).toArray
      case _ => throw new Exception(s"Expected JsArray but ${js.getClass}")
    inline def toJson(t: Array[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson).toSeq)


  given [T:JsValueMapper]: JsValueMapper[Seq[T]] with
    inline def fromJson(js: JsValue): Seq[T] = js match
        case x:JsArray => x.elements.map(summon[JsValueMapper[T]].fromJson)
        case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: Seq[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson) )

  given [T: JsValueMapper]: JsValueMapper[List[T]] with
    inline def fromJson(js: JsValue): List[T] = js match
      case x:JsArray => x.elements.map(summon[JsValueMapper[T]].fromJson).toList
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: List[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

  given [T: JsValueMapper]: JsValueMapper[Vector[T]] with
    inline def fromJson(js: JsValue): Vector[T] = js match
      case x:JsArray => x.elements.map(summon[JsValueMapper[T]].fromJson).toVector
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: Vector[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

  given [T: JsValueMapper]: JsValueMapper[Set[T]] with
    inline def fromJson(js: JsValue): Set[T] = js match
      case x:JsArray => x.elements.map(summon[JsValueMapper[T]].fromJson).toSet
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: Set[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson).toSeq:_*)

  given [T: JsValueMapper :Ordering]: JsValueMapper[SortedSet[T]] with
    inline def fromJson(js: JsValue): SortedSet[T] = js match
      case x:JsArray => SortedSet( x.elements.map(summon[JsValueMapper[T]].fromJson):_* )
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: SortedSet[T]): JsValue = JsArray(t.toList.map(summon[JsValueMapper[T]].toJson):_*)

  given [T:JsValueMapper]: JsValueMapper[Map[String, T]] with
    inline def fromJson(js: JsValue): Map[String, T] = js match
      case o:JsObject => o.fields.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toMap
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    inline def toJson(t: Map[String, T]): JsValue = JsObject( t.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T:JsValueMapper]: JsValueMapper[SortedMap[String, T]] with
    inline def fromJson(js: JsValue): SortedMap[String, T] = js match
      case o:JsObject => SortedMap( o.fields.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toSeq:_* )
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    inline def toJson(t: SortedMap[String, T]): JsValue = JsObject( t.toMap.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T: JsValueMapper]: JsValueMapper[Option[T]] with
    inline def fromJson(js: JsValue): Option[T] = js match
      case JsNull => None
      case _ => Some(summon[JsValueMapper[T]].fromJson(js))
    inline def toJson(t: Option[T]): JsValue = t match
      case x:Some[T]=> summon[JsValueMapper[T]].toJson(x.value)
      case None => JsNull

  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fields.get(name) match
      case x: Some[JsValue] if x.value ne JsNull => summon[JsValueMapper[T]].fromJson(x.value)
      case _ => throw new Exception("Expected field " + name + " not exists in JSON")

  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default:T): T =
    js.fields.get(name) match
      case x: Some[JsValue] => if x.value eq JsNull then default else summon[JsValueMapper[T]].fromJson(x.value)
      case _ => default

extension [T: JsValueMapper](obj: T)
  def toJson: JsValue = summon[JsValueMapper[T]].toJson(obj)

extension (js: JsValue)
  def convertTo[T: JsValueMapper]: T = summon[JsValueMapper[T]].fromJson(js)

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def rejson = new RejsonInterpolation(sc)

