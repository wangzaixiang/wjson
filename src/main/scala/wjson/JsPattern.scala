package wjson

/**
 * Json Pattern DSL
 */
enum JsPattern(bind: Option[String]):

  def unapplySeq(js: JsValue): Option[List[Any]] = JsPattern.unapplySeq(this, js)

  case JsNullPattern(bind: Option[String] = None) extends JsPattern(bind) // null literal
  case JsBoolPattern(value: Boolean, bind:Option[String] = None) extends JsPattern(bind)  // true or false literal
  case JsNumberPattern(value: Double, bind: Option[String] = None) extends JsPattern(bind)  // number literal
  case JsStringPattern(value: String, bind: Option[String] = None) extends JsPattern(bind)  // string literal
  case JsArrPattern(value: List[JsPattern], bind: Option[String] = None) extends JsPattern(bind) // array literal
  case JsObjPattern(value: Map[String, JsPattern], bind: Option[String] = None) extends JsPattern(bind) // object literal
  case JsBooleanType(bind: Option[String] = None) extends JsPattern(bind) // ground type: boolean
  case JsNumberType(bind: Option[String]=None) extends JsPattern(bind)  // ground type: number
  case JsStringType(bind: Option[String]=None) extends JsPattern(bind)  // ground type: string
  case JsAnyVal(bind: Option[String]=None) extends JsPattern(bind)      // _: match any JsVal
  case JsAnyVals(bind: Option[String] = None) extends JsPattern(bind)     // _*: match any number of JsVals
  case JsValuePattern(bind: Option[String] = None) extends JsPattern(bind) // id"pattern": match a specific JsVal

object JsPattern:
  def unapplySeq(pat: JsPattern, js: JsValue): Option[List[Any]] = ???
  def test(pat: JsPattern, js: JsValue): (Boolean, Map[String, Any]) = ???
  def parsePattern(string: String): JsPattern = ???
