package wjson

import scala.jdk.CollectionConverters.*
import scala.util.parsing.combinator.*
import wjson.JsPattern.*

class JsPatternParser extends RegexParsers:
  override def skipWhitespace = true
  override val whiteSpace = """([ \t\r\n]*(#[^\n]*\n)?)*""".r
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  def integer: Parser[String] =  """-?\d+""".r
  def float: Parser[String] = """-?\d+\.\d+""".r
  def string: Parser[String] =
    """"[^"]*"""".r ^^ { x=> x.substring(1, x.length-1) } |
    """'[^']*'""".r ^^ { x => x.substring(1, x.length-1) }
  def boolean: Parser[String] = """(true|false)""".r
  def `null`: Parser[String] = """null""".r

  def bind_jsval: Parser[Variable] = opt(binding) ~ jsval ^^ {
    case None ~ v => Variable(null, v)
    case Some(b) ~ v => Variable(b, v)
  }

  val TagPattern = """([a-zA-Z][a-zA-Z0-9_]*)"([^"]*)"""".r

  def jsval: Parser[JsPattern] = (
      `null` ^^ { x => JsPattern.NullPattern() }
      | boolean ^^ { x => JsPattern.BoolPattern(x.toBoolean) }
      | string ^^ { x => JsPattern.StringPattern(x) }
      | float ^^ { x => JsPattern.NumberPattern(x.toDouble) }
      | integer ^^ { x => JsPattern.NumberPattern(x.toDouble) }
      | type_string ^^ { x => JsPattern.AnyVal(GroundType.STRING) }
      | type_number ^^ {x => JsPattern.AnyVal(GroundType.NUMBER) }
      | type_boolean ^^ {x => JsPattern.AnyVal(GroundType.BOOLEAN) }
      | type_integer ^^ {x => JsPattern.AnyVal(GroundType.INTEGER) }
      | type_array ^^ {x => JsPattern.AnyVal(GroundType.ARRAY) }
      | type_object ^^ {x => JsPattern.AnyVal(GroundType.OBJECT) }
      | any1 ^^ {x => JsPattern.AnyVal(GroundType.ANY) }
      | tagedString ^^ {
        case TagPattern(tag, content) => JsPattern.TaggedString(tag, content)
      }
      | array  | `object` )
  def binding: Parser[String] = ident ~ "@" ^^ { case id ~ _ => id }
  def type_string: Parser[String] = "string"
  def type_number: Parser[String] = "number"
  def type_boolean: Parser[String] = "boolean"
  def type_integer: Parser[String] = "integer"
  def type_array: Parser[String] = "array"
  def type_object: Parser[String] = "object"
  def any1: Parser[String] = "_"
  def anys: Parser[String] = "_*"
  def tagedString: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*"[^"]*"""".r

  def array_item: Parser[Variable] = opt(binding) ~ anys ^^ {
    case None ~ _ => Variable(null, AnyVals())
    case Some(b) ~ _ => Variable(b, AnyVals())
  } | bind_jsval

  def array: Parser[ArrPattern] = "[" ~> repsep(array_item, ",") <~ opt(",") <~ "]" ^^ {
    case Nil => ArrPattern(Nil)
    case xs => ArrPattern(xs)
  }

  def `object`: Parser[JsPattern.ObjPattern] = "{" ~> repsep(field, ",") <~ opt(",") ~ "}" ^^ {
    case Nil => JsPattern.ObjPattern(Nil)
    case xs => JsPattern.ObjPattern(xs)
  }
  def field: Parser[(String,Variable)] =
    (path ~ ":" ~ bind_jsval) ^^ { case p ~ _ ~ v => (p, v) } |
    (opt(binding) ~ anys) ^^ { case binding ~ _ => (null, Variable(binding.getOrElse(null), AnyVals())) }
  def path: Parser[String] = string | ident ~ rep("/" ~> ident) ^^ {
    case id ~ Nil => id;
    case id ~ xs => id + "/" + xs.mkString("/")
  }


object JsPatternParser {

  def parseRejson(program: String): Variable =
    val parser = new JsPatternParser()
    parser.parse(parser.bind_jsval, program) match
      case parser.Success(result, _) => result
      case x@parser.Failure(msg, next) => throw new Exception("failed:" + x.toString)
      case x@parser.Error(msg, next) => throw new Exception("error:" + x.toString)

}
