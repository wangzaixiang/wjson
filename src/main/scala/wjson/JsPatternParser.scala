package wjson

import wjson.lexer.RejsonLexer
import wjson.parser.Program
import wjson.patterns.ast.Tree
import scala.jdk.CollectionConverters.*

object JsPatternParser {

  def buildPatterns(root: Tree.PatternWithName): wjson.PatternWithName =

    val p: JsPattern = root.pattern match
      case x: Tree.BooleanPattern =>
        JsPattern.BoolPattern(x.value)
      case x: Tree.StringPattern =>
        JsPattern.StringPattern(x.value)
      case x: Tree.NumberPattern =>
        JsPattern.NumberPattern(x.value)
      case x: Tree.NullPattern =>
        JsPattern.NullPattern()
      case x: Tree.ArrayPattern =>
        JsPattern.ArrPattern( x.value.asScala.toList.map(buildPatterns) )
      case x: Tree.ObjPattern =>
        JsPattern.ObjPattern( x.value.asScala.toMap.map { case(field, pwn) =>
          (field, buildPatterns(pwn))
        })
      case x: Tree.AnyVal =>
        val groundType = x.groundType match
          case "string" => GroundType.STRING
          case "number" => GroundType.NUMBER
          case "integer" => GroundType.INTEGER
          case "boolean" => GroundType.BOOLEAN
          case "array" => GroundType.ARRAY
          case "object" => GroundType.OBJECT
          case "any" => GroundType.ANY
        JsPattern.AnyVal(groundType)
      case x: Tree.TagedString =>
        JsPattern.TagedString(x.tag, x.value)
      case x: Tree.AnyVals =>
        JsPattern.AnyVals()
    wjson.PatternWithName(root.name, p)

  def main(args: Array[String]): Unit = {

    val string = """
      {  "name": uname@"John",
         "age": uage@30,
         "like": string,
         address/city: "guangzhou",
         "uid": mod"1024n+0..10",
         _*
      }
      """

      val lexer = new RejsonLexer(string)
      val program = new Program(lexer)
      val result = program.parse()

      val root = program.root

      println(s"result = $result, ${buildPatterns(root)}")

  }


}
