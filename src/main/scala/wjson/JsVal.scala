package wjson

enum JsVal:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double)
    case JsString(value: String)
    case JsArr(elements: List[JsVal])
    case JsObj(fields: Map[String, JsVal])


object JsVal:
  def parse(str: String): JsVal = JsonParser.parse(ParserInput(str))
  val JsTrue: JsBoolean = JsBoolean(true)
  val JsFalse: JsBoolean = JsBoolean(false)
  val JsZero: JsNumber = JsNumber(0)
  val JsEmptyString: JsString = JsString("")
  val JsEmptyObject: JsObj = JsObj()
  val JsEmptyArray: JsArr = JsArr()

  def JsObj(fields: (String, JsVal)*): JsObj = JsObj(Map(fields: _*))
  def JsArr(elements: JsVal*): JsArr = JsArr(elements.toList)

export JsVal.*

extension (str:String)
  def parseJson: JsVal = JsVal.parse(str)

trait JsValMapper[T]:
  def fromJson(js: JsVal): T
  def toJson(t: T): JsVal

given JsValMapper[Boolean] with
  def fromJson(js: JsVal): Boolean = js match
    case JsBoolean(value) => value
    case _ => throw new Exception("Expected JsBool")
  def toJson(t: Boolean): JsVal = JsBoolean(t)

given JsValMapper[Int] with
  override def fromJson(js: JsVal): Int = js match
    case JsNumber(value) => value.toInt
    case _ => throw new Exception("Expected JsNumber")
  override def toJson(t: Int): JsVal = JsNumber(t)

given Conversion[Int, JsVal] with
  override def apply(x: Int): JsVal = JsNumber(x)

given JsValMapper[Double] with
  override def fromJson(js: JsVal): Double = js match
    case JsNumber(value) => value
    case _ => throw new Exception("Expected JsNumber")

  override def toJson(t: Double): JsVal = ???

given JsValMapper[String] with
  override def fromJson(js: JsVal): String = js match
    case JsString(value) => value
    case _ => throw new Exception("Expected JsString")

  override def toJson(t: String): JsVal = JsString(t)
given Conversion[String, JsVal] with
  override def apply(x: String): JsVal = JsString(x)

given [T:JsValMapper]: JsValMapper[List[T]] with
  override def fromJson(js: JsVal): List[T] = js match
    case JsArr(value) => value.map(summon[JsValMapper[T]].fromJson)
    case _ => throw new Exception("Expected JsArr")
  override def toJson(t: List[T]): JsVal = ???

given [T:JsValMapper]: JsValMapper[Map[String, T]] with
  def fromJson(js: JsVal): Map[String, T] = js match
    case JsObj(value) => value.map( x => (x._1, summon[JsValMapper[T]].fromJson(x._2) ) ).toMap
    case _ => throw new Exception("Expected JsObj")
  def toJson(t: Map[String, T]): JsVal = ???

/**
 * given JsValMapper for case class T
 */
inline given [T](using deriving.Mirror.Of[T]): JsValMapper[T] =
  ${ JsValMapperMacro.generate[T] }

case class CaseField[T: JsValMapper](name: String, default: Option[T] = None):
  def apply(js: JsObj): T = js.fields.get(name) match
    case Some(value) => summon[JsValMapper[T]].fromJson(value)
    case None => default.getOrElse(throw new Exception("Expected field " + name))

extension [T: JsValMapper](obj: T)
  def toJson: JsVal = summon[JsValMapper[T]].toJson(obj)

extension (js: JsVal)
  def to[T: JsValMapper]: T = summon[JsValMapper[T]].fromJson(js)

private class rejson_sc(sc: StringContext):
  def unapplySeq(js: JsVal): Option[Seq[Any]] = Some(Seq("hello", 20, JsString("abc"), 100))  // ???

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def rejson = rejson_sc(sc)

