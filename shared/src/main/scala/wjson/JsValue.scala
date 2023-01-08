package wjson

import wjson.JsValue.{JsArray, JsNumber}

import scala.collection.{IterableOps, SortedMap, SortedSet, mutable}
import scala.reflect.ClassTag

/**
 * Json Model ADT
 */
enum JsValue:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double|Long)
    case JsString(value: String)
    case JsArray(elements: Seq[JsValue])
    case JsObject(fields: Map[String, JsValue])


object JsValue:
  val JsTrue: JsBoolean = JsBoolean(true)
  val JsFalse: JsBoolean = JsBoolean(false)
  val JsZero: JsNumber = JsNumber(0L)
  val JsEmptyString: JsString = JsString("")
  val JsEmptyObject: JsObject = JsObject()
  val JsEmptyArray: JsArray = JsArray()

  def parse(str: String): JsValue = JsonParser.parse(ParserInput(str))
  def JsObject(fields: (String, JsValue)*): JsObject = JsObject(Map(fields: _*))
  def JsArray(elements: JsValue*): JsArray = JsArray(elements.toList)
  def JsNumber(value: Int) = new JsNumber(value.toLong)

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
            if(buffer.endsWith(",\n")){
              buffer.delete(buffer.length() - 2, buffer.length()-1)
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
            if(buffer.endsWith(",\n")){
              buffer.delete(buffer.length() - 2, buffer.length()-1)
            }
            buffer.append(indentString).append("]")

      def escapedString(str: String): String =
        val sb = new StringBuilder()
        sb.append('"')
        str.foreach { c =>
          c match
            case '\\' => sb.append("\\\\")
            case '"' => sb.append("\\\"")
            case '\b' => sb.append("\\b")
            case '\f' => sb.append("\\f")
            case '\n' => sb.append("\\n")
            case '\r' => sb.append("\\r")
            case '\t' => sb.append("\\t")
            // case x if x > 0x100 => sb.append("\\u%04x".format(x.toInt))
            case _ => sb.append(c)
        }
        sb.append('"')
        sb.toString()

      show0(value, "")
      buffer.toString

  /**
   * support List,Seq,Vector,Set, SortedSet etc.
   */
  given [T: JsValueMapper, CC[x] <: IterableOps[x, CC, CC[x]]]: Conversion[CC[T], JsArray] with
    inline def apply(x: CC[T]): JsArray = JsArray( x.map(x => summon[JsValueMapper[T]].toJson(x)).toList:_* )

export JsValue.{JsNull, JsBoolean, JsNumber, JsString, JsArray, JsObject}

extension (str:String)
  def parseJson(`extension`: Boolean = false): JsValue = 
    if(`extension`) new JsonParser(ParserInput(str), true).parseJsValue()
    else JsValue.parse(str)


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
  inline def derived[T](using deriving.Mirror.Of[T]): JsValueMapper[T] =
    ${ JsValueMapperMacro.genADTImpl[T] }


  given JsValueMapper[JsValue] with
    inline def fromJson(js: JsValue): JsValue = js
    inline def toJson(t: JsValue): JsValue = t
  
  given JsValueMapper[JsBoolean] with
    inline def fromJson(js: JsValue): JsBoolean = (js: @unchecked) match
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

  given JsValueMapper[Boolean] with
    inline def fromJson(js: JsValue): Boolean = (js: @unchecked) match
      case x: JsBoolean => x.value
      case _ => throw new Exception(s"Expected JsBoolen but ${js.getClass}")
    inline def toJson(t: Boolean): JsValue = if(t) JsValue.JsTrue else JsValue.JsFalse

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
    inline def toJson(t:BigInt): JsValue = JsNumber(t.doubleValue)

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
    inline def toJson(t:java.math.BigInteger): JsValue = JsNumber(t.doubleValue)

  given JsValueMapper[String] with
    inline def fromJson(js: JsValue): String = (js: @unchecked) match
      case x:JsString => x.value
      case _ => throw new Exception(s"Expected JsString but ${js.getClass}")
    inline def toJson(t: String): JsValue = JsString(t)

  given [T:JsValueMapper :ClassTag]: JsValueMapper[Array[T]] with
    inline def fromJson(js: JsValue): Array[T] = (js: @unchecked) match
      case x:JsArray => x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)).toArray
      case _ => throw new Exception(s"Expected JsArray but ${js.getClass}")
    inline def toJson(t: Array[T]): JsValue = JsArray(t.map(x => summon[JsValueMapper[T]].toJson(x)).toSeq)


  trait Container[C[_]]:
    def fromSeq[T](src: Seq[T]): C[T]
    def toSeq[T](src: C[T]): Seq[T]

  given [T: JsValueMapper, C[_]: Container]: JsValueMapper[C[T]] with
    inline def fromJson(js: JsValue): C[T] = (js: @unchecked) match
      case x: JsArray => summon[Container[C]].fromSeq(x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)))
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: C[T]): JsValue = JsArray( summon[Container[C]].toSeq(t).map(x => summon[JsValueMapper[T]].toJson(x)) )

  given Container[Seq] with
    def fromSeq[T](src: Seq[T]) = src
    def toSeq[T](src: Seq[T]) = src

  given Container[List] with
    def fromSeq[T](src: Seq[T]): List[T] = src.toList
    def toSeq[T](src: List[T]):Seq[T] = src

  given Container[Vector] with
    def fromSeq[T](src: Seq[T]): Vector[T] = src.toVector
    def toSeq[T](src: Vector[T]): Seq[T] = src.toSeq

  given Container[Set] with
    def fromSeq[T](src: Seq[T]): Set[T] = src.toSet
    def toSeq[T](src: Set[T]): Seq[T] = src.toSeq

  given [T: JsValueMapper :Ordering]: JsValueMapper[SortedSet[T]] with
    inline def fromJson(js: JsValue): SortedSet[T] = (js: @unchecked) match
      case x:JsArray => SortedSet( x.elements.map(x => summon[JsValueMapper[T]].fromJson(x)):_* )
      case _ => throw new Exception(s"Expected JsArr but ${js.getClass}")
    inline def toJson(t: SortedSet[T]): JsValue = JsArray(t.toList.map(x => summon[JsValueMapper[T]].toJson(x)):_*)

  given [T:JsValueMapper]: JsValueMapper[Map[String, T]] with
    inline def fromJson(js: JsValue): Map[String, T] = (js: @unchecked) match
      case o:JsObject => o.fields.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toMap
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    inline def toJson(t: Map[String, T]): JsValue = JsObject( t.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T:JsValueMapper]: JsValueMapper[SortedMap[String, T]] with
    inline def fromJson(js: JsValue): SortedMap[String, T] = (js: @unchecked) match
      case o:JsObject => SortedMap( o.fields.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toSeq:_* )
      case _ => throw new Exception(s"Expected JsObj but ${js.getClass}")
    inline def toJson(t: SortedMap[String, T]): JsValue = JsObject( t.toMap.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

  given [T: JsValueMapper]: JsValueMapper[Option[T]] with
    inline def fromJson(js: JsValue): Option[T] = (js: @unchecked) match
      case JsNull => None
      case _ => Some(summon[JsValueMapper[T]].fromJson(js))
    inline def toJson(t: Option[T]): JsValue =  (t: @unchecked) match
      case x:Some[T]=> summon[JsValueMapper[T]].toJson(x.value)
      case None => JsNull

  /**
   * the T.derived has high priority than JsValMapper.given so if you define the derives,
   * this macro will not be used.
   */
  inline given[T](using deriving.Mirror.Of[T]): JsValueMapper[T] =
    ${ JsValueMapperMacro.genADTImpl[T] }


  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fields.get(name) match
      case x: Some[JsValue] if x.value ne JsNull => summon[JsValueMapper[T]].fromJson(x.value)
      case _ => throw new Exception("Expected field " + name + " not exists in JSON")

  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default:T): T =
    js.fields.get(name) match
      case x: Some[JsValue] => if x.value eq JsNull then default else summon[JsValueMapper[T]].fromJson(x.value)
      case _ => default

extension [T: JsValueMapper](obj: T)
  inline def toJson: JsValue = summon[JsValueMapper[T]].toJson(obj)

extension (js: JsValue)
  inline def convertTo[T: JsValueMapper]: T = summon[JsValueMapper[T]].fromJson(js)

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def rejson = new RejsonInterpolation(sc)

