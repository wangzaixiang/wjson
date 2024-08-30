package wjson.schema.generator

import wjson.*

import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}

object TastySchemaGenerator:

    // TODO generate all top level ADTs
    class JsonSchemaInspector extends Inspector:

        var schema: Option[JsObject] = None

        override def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
            import quotes.reflect.*
            val tasty = tastys(0)
            val tree = tasty.ast

            val toplevelADTType = tree match
                case PackageClause(pid, stats) =>
                    stats.flatMap:
                        case classDef@ClassDef(_, _, _, _, _) =>
                            val symbol = classDef.symbol
                            val isCase = symbol.flags.is(Flags.Case)
                            val isEnum = symbol.flags.is(Flags.Enum)
                            val isSynthetic = symbol.flags.is(Flags.Synthetic)

                            if isEnum && !isSynthetic then Some(symbol.typeRef)
                            else if isCase && !isSynthetic then Some(symbol.typeRef)
                            else None
                        case _ => None
            val definitions = collection.mutable.Set[TypeRepr]() ++ toplevelADTType
            val generator = new wjson.schema.generator.JsonSchemaGenerator.Generator(quotes)

            schema = toplevelADTType(0).asType match
                case '[t] => Some( generator.of[t] )

    def generateSchema(tastyPath: String, schemaPath: String) =
        val inspector = new JsonSchemaInspector
        TastyInspector.inspectTastyFiles(List(tastyPath))(inspector)
        inspector.schema.get.showPretty


