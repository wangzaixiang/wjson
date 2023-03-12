package wjson.schema

import wjson.JsValue.JsObject

import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}
import scala.quoted.*

object JsonSchemaGenerator:

  class JsonSchemaInspector extends Inspector:

    override def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*

      case class SumType(symbol: Symbol, items: List[ProductType])

      enum ProductType:
        case Single(symbol: Symbol)
        case Case(symbol: Symbol, fields: List[Symbol])

      val tree: Tree = tastys(0).ast

      val result: List[SumType|ProductType] = tree match
        case PackageClause(pid, stats) =>
          stats.flatMap {
            case classDef@ClassDef(name, constr, parents, selfOpt, body) => {
              val symbol = classDef.symbol
              val isCase = symbol.flags.is(Flags.Case)
              val isEnum = symbol.flags.is(Flags.Enum)
              val isSynthetic = symbol.flags.is(Flags.Synthetic)

              if isEnum && !isSynthetic then // SUM TYPE
                body.find { // find fitsy Import
                  case Import(_, _) => true
                  case _ => false
                } match
                  case Some(x@Import(term, selectors)) => // import Color.{Red, Green, Blue, RGB}
                    val items = selectors.map {
                      case SimpleSelector(name) =>
                        val isSingle = term.symbol.declaredType(name) == Nil // Red, Green, Blue
                        if isSingle then
                          ProductType.Single(term.symbol.declaredField(name))
                        else
                          val tpe = term.symbol.declaredType(name)(0)
                          val fields = tpe.caseFields
                          ProductType.Case(term.symbol.declaredField(name), fields)
                        end if
                      case _ => ???
                    }
                    val result = SumType(symbol, items)
                    Some(result)
                  case _ => None  // No import

              else if isCase && !isSynthetic then // PRODUCT TYPE
                val fields = symbol.caseFields
                Some(ProductType.Case(symbol, fields))
              else
                None
              end if
            }
            case _ => Nil
          }
        case _ => Nil

      println(result)
      result(0) match
        case sum @ SumType( typeSymbol, items ) =>
          println("SUM TYPE")
          import wjson.JsValue.*
          val jso = JsObject(
            "$schema" -> JsString("http://json-schema.org/draft-07/schema#"),
            "$id" -> JsString("http://example.com/example.json"),
            "oneOf" -> JsArray(
              items.map {
                case ProductType.Single(symbol) =>
                  JsObject(
                    "type" -> JsString("string"),
                    "const" -> JsString(symbol.name)
                  )
                case ProductType.Case(symbol, fields) =>
                  JsObject(
                    "type" -> JsString("object"),
                    "properties" -> JsObject(
                      fields.map { field =>
                        field.name -> JsObject(
                          "type" -> JsString("string")
                        )
                      }.toMap
                    )
                  )
              }
            )
          )
          println( jso.show() )



  def main(args: Array[String]): Unit =
    val inspector = new JsonSchemaInspector
    val tastyFiles = List("schema/target/scala-3.2.2/test-classes/wjson/schema/test/Color.tasty")
    TastyInspector.inspectTastyFiles(tastyFiles)(new JsonSchemaInspector)