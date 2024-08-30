package wjson.schema.generator

import wjson.{*, given}
import wjson.schema.JsonSchema

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*

object JsonSchemaGenerator:

    class Generator(quotes: Quotes):
        import quotes.reflect.*
        given Quotes = quotes

        private def extractDescription(symbol: Symbol): Option[String] =
            symbol.annotations
              .find(_.tpe =:= TypeRepr.of[JsonSchema.description])
              .map { case Apply(_, List(Literal(StringConstant(str)))) => str }


        private def schemaOf(tpe: TypeRepr, byRef: Boolean, definitions: mutable.Set[TypeRepr]): JsObject =
            tpe.asType match
                case '[t] =>
                    schemaOf[t](byRef, definitions)

        // generate a embedded schema for given type
        private def schemaOf[T: Type](byRef: Boolean, definitions: mutable.Set[TypeRepr]): JsObject =
            TypeRepr.of[T] match
                case x if x =:= TypeRepr.of[JsValue] =>
                    JsValue.obj("type" -> JsValue.arr("null", "boolean", "integer", "number", "array", "object"))
                case x if x =:= TypeRepr.of[JsValue.JsString] =>
                    JsValue.obj("type" -> "string".toJson)
                case x if x =:= TypeRepr.of[JsValue.JsNumber] =>
                    JsValue.obj("type" -> "number".toJson)
                case x if x =:= TypeRepr.of[JsValue.JsBoolean] =>
                    JsValue.obj("type" -> "boolean".toJson)
                case x if x =:= TypeRepr.of[JsValue.JsArray] =>
                    JsValue.obj("type" -> "array".toJson)
                case x if x =:= TypeRepr.of[JsValue.JsObject] =>
                    JsValue.obj("type" -> "object".toJson)

                case x if x <:< TypeRepr.of[String] =>
                    JsValue.obj("type" -> "string".toJson)
                case x if x =:= TypeRepr.of[Int] || x =:= TypeRepr.of[Short] || x =:= TypeRepr.of[Long] =>
                    JsValue.obj("type" -> "integer".toJson)
                case x if x =:= TypeRepr.of[Float] || x =:= TypeRepr.of[Double] =>
                    JsValue.obj("type" -> "number".toJson)
                case x if x =:= TypeRepr.of[Boolean] =>
                    JsValue.obj("type" -> "boolean".toJson)

                case x@AppliedType(base, args) if base <:< Symbol.requiredClass("wjson.schema.JsonSchema.Pointer").typeRef =>
                    val argTypeName = args(0).typeSymbol.fullName
                    JsValue.obj("type" -> "string", "$comment" -> s"pointer to ${argTypeName}")

                case x if x.typeSymbol.flags.is(Flags.Enum) && !x.typeSymbol.flags.is(Flags.Case)
                  && !x.typeSymbol.flags.is(Flags.Synthetic) =>
                    if byRef then
                        definitions.add(x)
                        JsValue.obj("$ref" -> s"#/definitions/${x.typeSymbol.fullName}".toJson)
                    else
                        println("enter enum:" + x.show)
                        schemaOfEnum(x, definitions)

                case x if x.typeSymbol.flags.is(Flags.Case) && !x.typeSymbol.flags.is(Flags.Synthetic) =>
                    if byRef then
                        definitions.add(x)
                        JsValue.obj("$ref" -> s"#/definitions/${x.typeSymbol.fullName}".toJson)
                    else
                        schemaOfProduct(x, definitions)

                case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.collection.immutable.List").typeRef =>
                    JsValue.obj("type" -> "array".toJson,
                        "items" -> schemaOf(args(0), true, definitions))

                case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.Array").typeRef =>
                    JsValue.obj("type" -> "array",
                        "items" -> schemaOf(args(0), true,  definitions))

                // TODO check Map[K, V]
                case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.collection.immutable.Map").typeRef =>
                    JsValue.obj("type" -> "object",
                        "additionalProperties" -> schemaOf(args(1), true, definitions))

                case x@AppliedType(base, args) if base <:< Symbol.requiredClass("scala.Option").typeRef =>
                    schemaOf(args(0), true, definitions)

                // TODO can we support or type? how to process the tag?
                case x@OrType(left, right) =>
                    val l = schemaOf(left, true, definitions)
                    val r = schemaOf(right, true, definitions)

                    val flatL =
                        if l.contains("oneOf") then l.field("oneOf").asArr.elements.toList
                        else List(l)

                    val flatR =
                        if r.contains("oneOf") then r.field("oneOf").asArr.elements.toList
                        else List(r)

                    JsValue.obj("oneOf" -> JsArray(flatL ++ flatR))

                case tpe@_ =>
                    val sym = TypeRepr.of[T].typeSymbol
                    if sym.isType then
                        sym.tree match
                            case TypeDef(_, rhs: Term) =>
                                schemaOf(rhs.symbol.typeRef, true, definitions)
                            case TypeDef(_, rhs: TypeTree) =>
                                schemaOf(rhs.tpe, true, definitions)
                            case _ =>
                                report.error(s"Unsupported type: ${tpe.show}")
                                ???
                    else
                        report.error(s"Unsupported type: ${tpe.show} ")
                        ???

        private def schemaOfEnum[T: Type](definitions: mutable.Set[TypeRepr]): JsObject =
            val symbol = TypeRepr.of[T].typeSymbol
            assert( symbol.flags is Flags.Enum )

            val choices = symbol.children.map:
                case x if x.isTerm => // Simple Case
                    JsValue.obj("const" -> x.name.toJson)
                case x if x.isType => // Product Case
                    val schema =
                        val inner = schemaOf(x.typeRef, false, definitions)

                        val properties = inner.field("properties").asObj +
                          ("_$tag" -> json5"{const: ${x.typeRef.typeSymbol.name}}")
                        val required = inner.field("required").asArr.elements :+ JsString("_$tag")
                        inner.merge( "properties" -> properties,
                            "required" -> required )

                    extractDescription(x) match
                        case Some(desc) => schema + ("description" -> desc)
                        case None => schema

            JsValue.obj("oneOf" -> choices.toJson)

        private def schemaOfEnum(tpe: TypeRepr, definitions: mutable.Set[TypeRepr]): JsObject =
            tpe.asType match
                case '[t] =>
                    schemaOfEnum[t](definitions)

        private def schemaOfProduct(tpe: TypeRepr, definitions: mutable.Set[TypeRepr]): JsObject =
            tpe.asType match
                case '[t] =>
                    schemaOfProduct[t](definitions)


        // TODO support default value in Schema?
        private case class FieldInfo(name: String, tpe: TypeRepr,
                                     symbol: Symbol,  // the field Symbol
                                     valdef: ValDef,  // the field init tree contains default value
                                     optional: Boolean
                                    )
        private def schemaOfProduct[T: Type](definitions: mutable.Set[TypeRepr]): JsObject =
            val typeSymbol = TypeRepr.of[T].typeSymbol

            val params: List[ValDef] = typeSymbol.primaryConstructor.tree.asInstanceOf[DefDef].paramss(0).params.asInstanceOf[List[ValDef]]

            val fieldInfos = typeSymbol.caseFields.zip(params).map :(field, param) =>
                val optional = param.tpt.tpe <:< TypeRepr.of[Option[_]]
                FieldInfo( name = field.name, tpe = param.tpt.tpe, symbol = field, valdef = param, optional = optional)

            val fields: List[(String, JsValue)] = fieldInfos.map :field =>
              val schema = schemaOf(field.tpe, true, definitions)
              extractDescription(field.valdef.symbol) match
                  case Some(desc) => (field.name, schema + ("description" -> desc) )
                  case None => (field.name, schema)

            val required = fieldInfos.filter(_.optional == false).map(f => JsString(f.name) )
            JsValue.obj(
                "type" -> "object",
                "properties" -> JsObject(fields),
                "required" -> required,
                "additionalProperties" -> false
            )

        @tailrec
        private def recur(root: JsObject, remains: mutable.Set[TypeRepr], processed: mutable.Set[TypeRepr]): JsObject =
            if remains.isEmpty then root
            else
                val (head, tail) = (remains.head, remains.tail)
                if processed.contains(head) then recur(root, tail, processed)
                else
                    val fullName = head.typeSymbol.fullName
                    val schema = schemaOf(head, false, remains)
                    val newRoot =
                        if root.contains("definitions") then
                            root + ("definitions" -> (root.field("definitions").asObj + (fullName -> schema)))
                        else
                            root + ("definitions" -> JsValue.obj(fullName -> schema))
                    recur(newRoot, remains - head, processed + head)

        // generate a JSON schema for given type
        def of[T: Type]: JsObject =
            val definitions = mutable.Set[TypeRepr]()
            val root = JsValue.obj(
                "$schema" -> "http://json-schema.org/draft-07/schema#",
                "$id" -> TypeRepr.of[T].typeSymbol.fullName
            ) ++ schemaOf[T](false, definitions)
            recur(root, definitions, mutable.Set())

    inline def of[T]: String = ${ ofImpl[T] }

    private def ofImpl[T: Type](using Quotes): Expr[String] =
        val generator = new Generator(quotes)
        val schema = generator.of[T]
        Expr(schema.showPretty)