package wjson.pattern

import scala.util.parsing.combinator.*
import JsPattern.*

import scala.util.matching.Regex

class JsPatternParser extends RegexParsers:
  override def skipWhitespace = true
  override val whiteSpace: Regex = """([ \t\r\n]*(#[^\n]*\n)?)*""".r
  private def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  private def integer: Parser[String] =  """-?\d+""".r
  private def float: Parser[String] = """-?\d+\.\d+""".r
  def string: Parser[String] =
    """"[^"]*"""".r ^^ { x=> x.substring(1, x.length-1) } |
    """'[^']*'""".r ^^ { x => x.substring(1, x.length-1) }
  private def boolean: Parser[String] = """(true|false)""".r
  def `null`: Parser[String] = """null""".r

  def bind_jsval: Parser[Variable] = opt(binding) ~ jsval ^^ {
    case None ~ v => Variable(null, v)
    case Some(b) ~ v => Variable(b, v)
  }

  private val TagPattern1 = """([a-zA-Z][a-zA-Z0-9_]*)"([^"]*)"""".r
  private val TagPattern2 = """([a-zA-Z][a-zA-Z0-9_]*)'([^']*)'""".r

  private def jsval: Parser[JsPattern] =
      `null` ^^^ { JsPattern.NullPattern() }
      | boolean ^^ { x => JsPattern.BoolPattern(x.toBoolean) }
      | string ^^ { x => JsPattern.StringPattern(x) }
      | float ^^ { x => JsPattern.NumberPattern(x.toDouble) }
      | integer ^^ { x => JsPattern.NumberPattern(x.toLong) }
      | type_string ^^^ { JsPattern.AnyVal(GroundType.STRING) }
      | type_number ^^^ { JsPattern.AnyVal(GroundType.NUMBER) }
      | type_boolean ^^^ { JsPattern.AnyVal(GroundType.BOOLEAN) }
      | type_integer ^^^ { JsPattern.AnyVal(GroundType.INTEGER) }
      | type_array ^^^ { JsPattern.AnyVal(GroundType.ARRAY) }
      | type_object ^^^ { JsPattern.AnyVal(GroundType.OBJECT) }
      | any1 ^^^ { JsPattern.AnyVal(GroundType.ANY) }
      | taggedString ^^ { x => JsPattern.TaggedString(x._1, x._2) }
      | array | `object`

  private def binding: Parser[String] = ident ~ "@" ^^ { case id ~ _ => id }
  private def type_string: Parser[String] = "string"
  private def type_number: Parser[String] = "number"
  private def type_boolean: Parser[String] = "boolean"
  private def type_integer: Parser[String] = "integer"
  private def type_array: Parser[String] = "array"
  private def type_object: Parser[String] = "object"
  private def any1: Parser[String] = "_"
  private def anys: Parser[String] = "_*"
  private def taggedString: Parser[(String, String)] = """[a-zA-Z][a-zA-Z0-9_]*"[^"]*"""".r ^^ {
      case TagPattern1(tag, content) => (tag, content)
    }  | """[a-zA-Z][a-zA-Z0-9_]*'[^']*'""".r ^^ {
      case TagPattern2(tag, content) => (tag, content)
    }

  private def array_item: Parser[Variable] = opt(binding) ~ anys ^^ {
    case None ~ _ => Variable(null, AnyVals())
    case Some(b) ~ _ => Variable(b, AnyVals())
  } | bind_jsval

  private def array: Parser[ArrPattern] = "[" ~> repsep(array_item, ",") <~ opt(",") <~ "]" ^^ {
    case Nil => ArrPattern(Nil)
    case xs => ArrPattern(xs)
  }

  private def `object`: Parser[JsPattern.ObjPattern] = "{" ~> repsep(field, ",") <~ opt(",") ~ "}" ^^ {
    case Nil => JsPattern.ObjPattern(Nil)
    case xs => JsPattern.ObjPattern(xs)
  }
  private def field: Parser[(JsPattern.Path,Variable)] =
    (path ~ ":" ~ bind_jsval) ^^ { case p ~ _ ~ v => (p, v) } |
    (opt(binding) ~ anys) ^^ { case binding ~ _ => (null, Variable(binding.orNull, AnyVals())) }

  def path: Parser[JsPattern.Path] = string ^^ { x => Path( List(PathElement.Simple(x)) ) } |
    ident ~ rep(path_next)  ^^ {
      case id ~ path => Path( List( PathElement.Simple(id) ) ++ path )
    }
  private def path_next: Parser[PathElement] = '/' ~> ident ^^ {x => PathElement.Simple(x) } |
    "/*" ^^ { _ => PathElement.ArrayFilter( JsPattern.AnyVal(GroundType.ANY) ) } |
    '[' ~> jsval <~ ']' ^^ {
      case NumberPattern(value: Long) =>
        PathElement.Index(value.toInt)
      case x@_ =>  PathElement.ArrayFilter(x)
    }


object JsPatternParser {

  def parseJsPattern(program: String): Variable =
    val parser = new JsPatternParser()
    parser.parse(parser.bind_jsval, program) match
      case parser.Success(result, _) => result
      case x: parser.Failure => throw new Exception("failed:" + x.toString)
      case x: parser.Error => throw new Exception("error:" + x.toString)

}
