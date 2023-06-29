package wjson.pattern

import wjson.*

/**
 * Json Pattern DSL
 */
enum JsPattern:

  def unapplySeq(js: JsValue): Option[List[Any]] = JsPattern.unapplySeq(this, js)

  case NullPattern()  // null literal
  case BoolPattern(value: Boolean)   // true or false literal
  case NumberPattern(value: Double|Long)   // number literal
  case StringPattern(value: String)   // string literal
  case ArrPattern(value: List[JsPattern.Variable])  // array literal
  case ObjPattern(value: List[(JsPattern.Path, JsPattern.Variable)])  // object literal
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

  enum PathElement:
    case Simple(value: String)  // /name
    case ArrayFilter(filter: JsPattern) // [ pattern ]
    case Index(value: Int) // [0]

  case class Path(value: List[PathElement])

  case class Variable(name: String, pattern: JsPattern)

  def ObjPattern(fields: (String, Variable)*): ObjPattern =
    new ObjPattern(fields.toList.map{ p =>
      if(p._1 == null) (null: Path) -> p._2
      else Path(p._1) -> p._2
    })

  def unapplySeq(pat: JsPattern, js: JsValue): Option[List[Any]] = ???
  def test(pat: JsPattern, js: JsValue): (Boolean, Map[String, Any]) = ???
  def parsePattern(string: String): JsPattern = ???

  def simplePath(path: String) = Path(List(PathElement.Simple(path)))
  def Path(path: String): Path =
    val parser = new JsPatternParser
    parser.parse( parser.path, path ) match
      case parser.Success(result, _) => result
      case x@parser.Failure(msg, next) => throw new Exception(s"invalid path $path : $msg")
      case x@parser.Error(msg, next) => throw new Exception(s"invalid path $path : $msg")

extension (sc: StringContext)
  def jsonp = new JsPatternInterpolation(sc)
