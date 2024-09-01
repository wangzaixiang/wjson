package wjson.schema.generator

import wjson.*

import scala.quoted.Quotes
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}

object TastySchemaGenerator:

    class JsonSchemaInspector extends Inspector:

        var schema: Option[JsObject] = None

        override def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
            import quotes.reflect.*

            assert(tastys.length == 1, "Only one tasty file is supported, the first one is used as root.")
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
            val definitions = toplevelADTType.map(_.asType).toList
            val generator = new wjson.schema.generator.JsonSchemaGenerator.Generator(quotes)

            schema = toplevelADTType(0).asType match
                case '[t] => Some( generator.of[t](definitions) )

    @main
    def tasty2schema(tastyPath: String, schemaPath: String): Unit =
        val inspector = new JsonSchemaInspector
        TastyInspector.inspectTastyFiles(List(tastyPath))(inspector)
        inspector.schema match
            case Some(schema) =>
                val schema = inspector.schema.get.showPretty

                val isConsole = schemaPath == "-"

                val writer = if isConsole then new java.io.PrintWriter(Console.out)
                             else new java.io.PrintWriter(schemaPath)
                writer.write(schema)
                writer.flush()

                // when running inside sbt, don't close the console
                if !isConsole then writer.close()
            case None =>
                println("tasty file parsed failed, maybe need classpath for dependencies")
        



