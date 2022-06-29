package wjson

import scala.quoted.*
import scala.deriving.*

/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object JsValueMapperMacro:

  inline def generate[T: deriving.Mirror.ProductOf]: JsValueMapper[T] =
    ${ generateImpl[T] }

  def generateImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    import JsValueMapper.*

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
    def getField(jso: Expr[JsObject], field: Symbol, THIS: Expr[JsValueMapper[T]]): Term =
      val name = field.name
      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
          // TODO better way to get the '{this} expr
          val summonValue: Option[Expr[JsValueMapper[t]]] =
            if(TypeRepr.of[t].widen =:= TypeRepr.of[T].widen) Some(THIS.asInstanceOf[Expr[JsValueMapper[t]]])
            else Expr.summon[JsValueMapper[t]]
          summonValue match
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
    def getFieldAsKV(value: Expr[T], field: Symbol, THIS: Expr[JsValueMapper[T]]): Expr[(String, JsValue)] = // (String, JsValue)
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[t] =>
            val isPrim  = isPrimitive(TypeRepr.of[t])
            val summonValue: Option[Expr[JsValueMapper[t]]] =
              if(TypeRepr.of[t].widen =:= TypeRepr.of[T].widen) Some(THIS.asInstanceOf[Expr[JsValueMapper[t]]])
              else Expr.summon[JsValueMapper[t]]
            summonValue match
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

    def buildBeanFrom(jso: Expr[JsObject], THIS: Expr[JsValueMapper[T]]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Term] = tpeSym.caseFields.map( field => getField(jso, field, THIS) )
      val constructor = tpeSym.primaryConstructor

      ValDef.let( Symbol.spliceOwner, terms ) { refs =>
        Apply( Select( New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    def buildJsVal(value: Expr[T], THIS: Expr[JsValueMapper[T]]): Expr[JsValue] =
      val tpeSym = TypeTree.of[T].symbol
      val terms: List[Expr[(String, JsValue)]] = tpeSym.caseFields.map( field => getFieldAsKV(value, field, THIS) )
      val asSeq = Expr.ofSeq(terms)
      '{ JsObject( (${asSeq}).filter(_._2 != JsNull):_* ) }

    val expr: Expr[?] = '{
      new JsValueMapper[T]:
        def fromJson(json: JsValue): T =
          val jso = json.asInstanceOf[JsObject]
          val THIS: JsValueMapper[T] = this
          ${ buildBeanFrom('{jso}, '{THIS}) }
        def toJson(value: T): JsValue =
          val THIS: JsValueMapper[T] = this
          ${ buildJsVal('{value}, '{THIS} ) }
    }

//    println("expr = " + expr.show)
//    println("expr.tree" + expr.asTerm.show(using Printer.TreeStructure))

    expr.asInstanceOf[Expr[JsValueMapper[T]]]


