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
  case ArrPattern(value: List[JsPattern.Variable])  // array literal
  case ObjPattern(value: Map[String, JsPattern.Variable])  // object literal
  case BooleanType()  // ground type: boolean
  case NumberType()   // ground type: number
  case StringType()   // ground type: string
  case AnyVal(ground: JsPattern.GroundType)       // _: match any JsVal
  case AnyVals()      // _*: match any number of JsVals
  case TaggedString(tag:String, content:String)  // id"pattern": match a specific JsVal

object JsPattern:
  enum GroundType:
    case STRING
    case NUMBER
    case INTEGER
    case BOOLEAN
    case ARRAY
    case OBJECT
    case ANY
  case class Variable(name: String, pattern: JsPattern)

  def unapplySeq(pat: JsPattern, js: JsValue): Option[List[Any]] = ???
  def test(pat: JsPattern, js: JsValue): (Boolean, Map[String, Any]) = ???
  def parsePattern(string: String): JsPattern = ???
