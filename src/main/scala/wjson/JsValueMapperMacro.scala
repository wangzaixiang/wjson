package wjson

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object JsValueMapperMacro:

  def generate[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*

    // for fields with default values, extract the default value
    val defaultParams: Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    // '{ new CaseField[t](field, default).apply(jso) }'
    def getField(jso: Expr[JsObject], field: Symbol): Term =
      val name = field.name
      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          Expr.summon[JsValueMapper[t]] match
            case Some(mapper) =>
              defaultParams.get(name) match
                case Some(deff) =>
                  ' { caseFieldGet[t]($jso, ${Expr(name)}, ${deff.asInstanceOf[Expr[t]]})(using $mapper) }
                case None =>
                  ' { caseFieldGet[t]($jso, ${Expr(name)})(using $mapper) }
            case None =>
              report.error(s"No JsValueMapper found, owner: ${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      expr.asTerm

    def isPrimitive(tpe: TypeRepr): Boolean =
      val anyVal = TypeRepr.of[AnyVal]
      tpe <:< anyVal

    // '{ (field.name, summon[JsValueMapper[field.type].toJson(value)]) }
    def getFieldAsKV(value: Expr[T], field: Symbol): Expr[(String, JsValue)] = // (String, JsValue)
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
            val isPrim  = isPrimitive(TypeRepr.of[t])
            Expr.summon[JsValueMapper[t]] match
              case Some(mapper) =>
                // (field.name, mapper.toJson( value.field ))
                val select = Select(value.asTerm, field).asExpr.asInstanceOf[Expr[t]]
                val json =
                  if(isPrim) '{ $mapper.toJson($select) }
                  else '{ if($select == null) JsNull else ${mapper}.toJson($select) }

                '{ ( ${Expr(field.name)}, ${json} ) }
              case None =>
                report.error(s"No JsValueMapper found, owner: ${TypeTree.of[T].show} field:${field.name} type:${TypeTree.of[t].show}")
                '{ (${Expr(field.name)}, ??? ) }

    def buildBeanFrom(jso: Expr[JsObject]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Term] = tpeSym.caseFields.map( field => getField(jso, field) )
      val constructor = tpeSym.primaryConstructor

      ValDef.let( Symbol.spliceOwner, terms ) { refs =>
        Apply( Select( New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    def buildJsVal(value: Expr[T]): Expr[JsValue] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Expr[(String, JsValue)]] = tpeSym.caseFields.map( field => getFieldAsKV(value, field) )
      val asSeq = Expr.ofSeq(terms)
      '{ JsObject( ${asSeq}:_* ) }

    val expr = '{
      new JsValueMapper[T]:
        def fromJson(json: JsValue): T =
          val jso = json.asInstanceOf[JsObject]
          ${ buildBeanFrom('{jso}) }
        def toJson(value: T): JsValue = ${ buildJsVal('{value} ) }
    }

    expr.asInstanceOf[Expr[JsValueMapper[T]]]

