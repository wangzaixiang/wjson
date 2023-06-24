package wjson.macros

import wjson.macros

import macros.ADTMappingMacro.*
import scala.quoted.*
import wjson.*

object ProductGenerator:

  private inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] if x.value ne JsNull => summon[JsValueMapper[T]].fromJson(x.value)
      case _ => throw new Exception("Expected field " + name + " not exists in JSON")

  private inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default: T): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] => if x.value eq JsNull then default else summon[JsValueMapper[T]].fromJson(x.value)
      case _ => default


class ProductGenerator[T: Type] extends Generator[T]:
  import ProductGenerator.*

  override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    val defaultParams: Map[String, Expr[Any]] = extractDefaultCaseParams[T]

    def buildBeanFrom(jso: Expr[JsObject]): Expr[T] =
      import quotes.reflect.*
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Term] = tpeSym.caseFields.map(field => getField(jso, field))
      val constructor = tpeSym.primaryConstructor

      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        Apply(Select(New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    def buildJsVal(value: Expr[T]): Expr[JsValue] =
      import quotes.reflect.*
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Expr[(String, JsValue)]] = tpeSym.caseFields.map(field => getFieldAsKV(value, field))
      val asSeq = Expr.ofSeq(terms)
      '{ JsObject((${ asSeq }).filter(_._2 != JsNull): _*) }

    // '{ new CaseField[t](field, default).apply(jso) }'
    def getField(jso: Expr[JsObject], field: Symbol): Term =
      val name = field.name
      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          val summonValue = summonJsValueMapper[t](deps)
          val isOptional = TypeRepr.of[None.type] <:< TypeRepr.of[t]
          val isNullable = TypeRepr.of[Null] <:< TypeRepr.of[t]

          summonValue match
            case Some(mapper) =>
              import ADTMappingMacro.*
              defaultParams.get(name) match
                case Some(_default) =>
                  '{ caseFieldGet[t]($jso, ${ Expr(name) }, ${ _default.asInstanceOf[Expr[t]] })(using $mapper) }
                case None if isOptional =>
                  '{ caseFieldGet[t]($jso, ${ Expr(name) }, None.asInstanceOf[t])(using $mapper) }
                case None if isNullable =>
                  '{ caseFieldGet[t]($jso, ${ Expr(name) }, null.asInstanceOf[t])(using $mapper) }
                case None => // no default value
                  '{ caseFieldGet[t]($jso, ${ Expr(name) })(using $mapper) }

            case None =>
              report.error(s"No JsValueMapper found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }

      expr.asTerm

    // '{ (field.name, summon[JsValueMapper[field.type].toJson(value)]) }
    def getFieldAsKV(value: Expr[T], field: quotes.reflect.Symbol): Expr[(String, JsValue)] =
      import quotes.reflect.*
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          val isPrimitive = TypeRepr.of[t] <:< TypeRepr.of[AnyVal]
          val summonValue = summonJsValueMapper[t](deps)

          summonValue match
            case Some(mapper) =>
              val select = Select(value.asTerm, field).asExpr.asInstanceOf[Expr[t]]
              val json =
                if isPrimitive then '{ ${ mapper }.toJson(${ select }) }
                else '{ if ${ select } == null then JsNull else ${ mapper }.toJson($select) }

              '{ (${ Expr(field.name) }, ${ json }) }
            case None =>
              report.error(s"No JsValueMapper found, owner:${TypeTree.of[T].show} field:${field.name} type:${TypeTree.of[t].show}")
              '{ (${ Expr(field.name) }, ???) }

    val expr: Expr[?] = '{
      new JsValueMapper[T]:
        def fromJson(json: JsValue): T =
          val jso = json.asInstanceOf[JsObject]
          ${ buildBeanFrom('{ jso }) }

        def toJson(value: T): JsValue =
          ${ buildJsVal('{ value }) }
    }

    expr.asInstanceOf[Expr[JsValueMapper[T]]]

