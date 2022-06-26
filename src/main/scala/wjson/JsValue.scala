package wjson

/**
 * Json Model ADT
 */
enum JsValue:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double)
    case JsString(value: String)
    case JsArray(elements: List[JsValue])
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

export JsValue.{JsNull, JsBoolean, JsNumber, JsString, JsArray, JsObject}

extension (str:String)
  def parseJson: JsValue = JsValue.parse(str)

/**
 * type class for JsValue Mapping
 */
trait JsValueMapper[T]:
  def fromJson(js: JsValue): T
  def toJson(t: T): JsValue

given JsValueMapper[Boolean] with
  def fromJson(js: JsValue): Boolean = js match
    case JsBoolean(value) => value
    case _ => throw new Exception("Expected JsBoolen")
  def toJson(t: Boolean): JsValue = if(t) JsValue.JsTrue else JsValue.JsFalse

given JsValueMapper[Byte] with
  def fromJson(js: JsValue): Byte = js match
    case JsNumber(value) => value.toByte
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Byte): JsValue = JsNumber(t)

given JsValueMapper[Short] with
  def fromJson(js: JsValue): Short = js match
    case JsNumber(value) => value.toShort
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Short): JsValue = JsNumber(t)

given JsValueMapper[Int] with
  def fromJson(js: JsValue): Int = js match
    case JsNumber(value) => value.toInt
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Int): JsValue = JsNumber(t)

given JsValueMapper[Long] with
  def fromJson(js: JsValue): Long = js match
    case JsNumber(value) => value.toLong
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Long): JsValue = JsNumber(t)

given JsValueMapper[Float] with
  def fromJson(js: JsValue): Float = js match
    case JsNumber(value) => value.toFloat
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Float): JsValue = JsNumber(t)

given JsValueMapper[Double] with
  def fromJson(js: JsValue): Double = js match
    case JsNumber(value) => value
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: Double): JsValue = JsNumber(t)

given JsValueMapper[BigDecimal] with
  def fromJson(js: JsValue): BigDecimal = js match
    case JsNumber(value) => BigDecimal(value)
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: BigDecimal): JsValue = JsNumber(t.doubleValue)

given JsValueMapper[BigInt] with
  def fromJson(js: JsValue): BigInt = js match
    case JsNumber(value) => BigInt(value.toString)
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t:BigInt): JsValue = JsNumber(t.doubleValue)

given jmBigDecimal: JsValueMapper[java.math.BigDecimal] with
  def fromJson(js: JsValue): java.math.BigDecimal = js match
    case JsNumber(value) => java.math.BigDecimal(value)
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t: java.math.BigDecimal): JsValue = JsNumber(t.doubleValue)

given JsValueMapper[java.math.BigInteger] with
  def fromJson(js: JsValue): java.math.BigInteger = js match
    case JsNumber(value) => java.math.BigInteger(value.toString)
    case _ => throw new Exception("Expected JsNumber")
  def toJson(t:java.math.BigInteger): JsValue = JsNumber(t.doubleValue)

given JsValueMapper[String] with
  def fromJson(js: JsValue): String = js match
    case JsString(value) => value
    case _ => throw new Exception("Expected JsString")
  def toJson(t: String): JsValue = JsString(t)

given [T:JsValueMapper]: JsValueMapper[Seq[T]] with
  def fromJson(js: JsValue): Seq[T] = js match
    case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson)
    case _ => throw new Exception("Expected JsArr")
  def toJson(t: Seq[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

given [T: JsValueMapper]: JsValueMapper[List[T]] with
  def fromJson(js: JsValue): List[T] = js match
    case JsArray(value) => value.map(summon[JsValueMapper[T]].fromJson).toList
    case _ => throw new Exception("Expected JsArr")
  def toJson(t: List[T]): JsValue = JsArray(t.map(summon[JsValueMapper[T]].toJson):_*)

given [T:JsValueMapper]: JsValueMapper[Map[String, T]] with
  def fromJson(js: JsValue): Map[String, T] = js match
    case JsObject(value) => value.map( x => (x._1, summon[JsValueMapper[T]].fromJson(x._2) ) ).toMap
    case _ => throw new Exception("Expected JsObj")
  def toJson(t: Map[String, T]): JsValue = JsObject( t.map( x => (x._1, summon[JsValueMapper[T]].toJson(x._2)) ) )

given [T: JsValueMapper]: JsValueMapper[Option[T]] with
  def fromJson(js: JsValue): Option[T] = js match
    case JsNull => None
    case _ => Some(summon[JsValueMapper[T]].fromJson(js))
  def toJson(t: Option[T]): JsValue = t match
    case Some(x) => summon[JsValueMapper[T]].toJson(x)
    case None => JsNull

given [T: JsValueMapper]: Conversion[T, JsValue] with
  def apply(x: T): JsValue = summon[JsValueMapper[T]].toJson(x)

given [T: JsValueMapper]: Conversion[List[T], JsValue] with
  def apply(x: List[T]): JsArray = JsArray( x.map(summon[JsValueMapper[T]].toJson):_* )

/**
 * given JsValueMapper for case class T
 */
inline given [T](using deriving.Mirror.ProductOf[T]): JsValueMapper[T] =
  ${ JsValueMapperMacro.generate[T] }

private inline def caseFieldGet[T: JsValueMapper](inline js: JsObject, inline name: String): T =
  js.fields.get(name) match
    case Some(value) => summon[JsValueMapper[T]].fromJson(value)
    case None => throw new Exception("Expected field " + name)

private inline def caseFieldGet[T: JsValueMapper](inline js: JsObject, inline name: String, inline default:T): T =
  js.fields.get(name) match
    case Some(value) => summon[JsValueMapper[T]].fromJson(value)
    case None => default

extension [T: JsValueMapper](obj: T)
  def toJson: JsValue = summon[JsValueMapper[T]].toJson(obj)

extension (js: JsValue)
  def to[T: JsValueMapper]: T = summon[JsValueMapper[T]].fromJson(js)

private class rejson_sc(sc: StringContext):
  def unapplySeq(js: JsValue): Option[Seq[Any]] = Some(Seq("hello", 20, JsString("abc"), 100))  // ???

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def rejson = rejson_sc(sc)

