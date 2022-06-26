package wjson

/**
 * Json Pattern DSL
 */
enum JsPattern:

  def unapplySeq(js: JsValue): Option[List[Any]] = JsPattern.unapplySeq(this, js)

  case NullPattern()  // null literal
  case BoolPattern(value: Boolean)   // true or false literal
  case NumberPattern(value: Double)   // number literal
  case StringPattern(value: String)   // string literal
  case ArrPattern(value: List[PatternWithName])  // array literal
  case ObjPattern(value: Map[String, PatternWithName])  // object literal
  case BooleanType()  // ground type: boolean
  case NumberType()   // ground type: number
  case StringType()   // ground type: string
  case AnyVal(ground: GroundType)       // _: match any JsVal
  case AnyVals()      // _*: match any number of JsVals
  case TagedString(tag:String, content:String)  // id"pattern": match a specific JsVal

enum GroundType:
  case STRING
  case NUMBER
  case INTEGER
  case BOOLEAN
  case ARRAY
  case OBJECT
  case ANY

case class PatternWithName(name: String, pattern: JsPattern)

object JsPattern:
  def unapplySeq(pat: JsPattern, js: JsValue): Option[List[Any]] = ???
  def test(pat: JsPattern, js: JsValue): (Boolean, Map[String, Any]) = ???
  def parsePattern(string: String): JsPattern = ???
