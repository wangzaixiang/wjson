package wjson

import com.sun.xml.internal.bind.v2.model.core.TypeRef
import wjson.JsValue.{JsBoolean, JsString}

import scala.deriving.*
import scala.quoted.*
import wjson.{*, given}

import scala.Symbol as _
import scala.annotation.experimental


/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object ADTMappingMacro:

  def genADTImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    new ADTMappingMacro(quotes).genADTImpl[T]

  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] if x.value ne JsNull => summon[JsValueMapper[T]].fromJson(x.value)
      case _ => throw new Exception("Expected field " + name + " not exists in JSON")

  inline def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default: T): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] => if x.value eq JsNull then default else summon[JsValueMapper[T]].fromJson(x.value)
      case _ => default

  private val NO_EXPAND_ADT = new ThreadLocal[Boolean]:
    override def initialValue(): Boolean = false

  // use ThreadLocal to trace the recursive calls
  private val NOT_EXPAND_ADTS = new ThreadLocal[ Int ]():
    override def initialValue(): Int = 0

  /**
   * to enable macro debug, runs like `sbt -Dwjson.printMacroCode=true compile`
   */
  val PRINT_MACRO_CODE: Boolean = java.lang.Boolean.getBoolean("wjson.printMacroCode")

class ADTMappingMacro(q: Quotes):

  private def extractTypeReprs[T: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    Type.of[T] match
      case '[t *: ts] => quotes.reflect.TypeRepr.of[t] :: extractTypeReprs[ts]
      case '[EmptyTuple] => Nil

  private def extractNames[T: Type](using Quotes): List[String] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: ts] => TypeRepr.of[t] match
        case ConstantType(StringConstant(name)) =>
          name :: extractNames[ts]
        case _ => throw new AssertionError("Expected a String constant type")
      case '[EmptyTuple] => Nil

  private def genADTImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =

    if ADTMappingMacro.NO_EXPAND_ADT.get().nn then
      ADTMappingMacro.NOT_EXPAND_ADTS.set(ADTMappingMacro.NOT_EXPAND_ADTS.get().nn + 1)
      '{ ??? }
    else
      val dependencies =
        try
          ADTMappingMacro.NO_EXPAND_ADT.set(true)
          Expr.summon[Mirror.Of[T]].get match
            case '{ $m: Mirror.ProductOf[T] } =>
                visit[T](Map.empty )
            case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames } } =>
              visit[T](Map.empty )
            case _ => throw new AssertionError("Expected a Product or a Sum")
        finally
          ADTMappingMacro.NO_EXPAND_ADT.set(false)

      if ADTMappingMacro.PRINT_MACRO_CODE then
        println("!!! dependencies: " + dependencies)
      genMultiMapperBlock[T](dependencies)

  private def extractDefaultParams[T: Type](using Quotes): Map[String, Expr[Any]] =
    import quotes.reflect.*

    val sym = TypeTree.of[T].symbol
    val comp = sym.companionClass
    if (comp == Symbol.noSymbol)
      throw new AssertionError("no companionClass found for type:" + TypeRepr.of[T].show(using Printer.TypeReprCode))
    val module = Ref(sym.companionModule)

    val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Expr[?]] = for case deff@DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
      yield module.select(deff.symbol).asExpr

    names.zip(idents).toMap

  private def genMultiMapperBlock[T: Type](needGenTypes: GeneratorMap)(using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    var i = 0  // variable name counter
    val valSyms: Map[TypeRepr, Symbol] = needGenTypes map: (tpe, generator) =>
      i += 1
      val sym = Symbol.newVal( Symbol.spliceOwner, s"mapper_${i}",  generator.mapperTpe, Flags.Lazy, Symbol.noSymbol )
      ( tpe.asInstanceOf[TypeRepr], sym )

    val refs: Map[TypeRepr, TermBuilder] = valSyms map : (tpe, sym) =>
      val builder = new TermBuilder:
        def make(using innerQuotes:Quotes): innerQuotes.reflect.Ref = innerQuotes.reflect.Ref(sym.asInstanceOf[innerQuotes.reflect.Symbol])
      ( tpe, builder)

    val valDefs: Map[TypeRepr, ValDef] = needGenTypes map: (tpe, generator) =>
      val sym: Symbol = valSyms( tpe.asInstanceOf[TypeRepr] )

      val newQuotes = sym.asQuotes // quotes
      val refs2 = refs.asInstanceOf[Map[newQuotes.reflect.TypeRepr, TermBuilder]]
      val mapper: Term = generator.generate(using newQuotes)(refs2).asTerm
      val valDef = ValDef( sym, Some(mapper) )
      (tpe.asInstanceOf[TypeRepr], valDef)

    val term  = Block(valDefs.values.toList, refs(TypeRepr.of[T]).make )

    if ADTMappingMacro.PRINT_MACRO_CODE then
      println("generated JsValueMapper[" + TypeRepr.of[T].show(using Printer.TypeReprAnsiCode) + "] = "
        + term.show(using Printer.TreeAnsiCode))

    term.asExpr.asInstanceOf[Expr[JsValueMapper[T]]]

  private trait TermBuilder:
    def make(using Quotes): quotes.reflect.Term

  private trait Generator[T: Type]:
    def baseTpe(using quotes: Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[T]
    def mapperTpe(using quotes: Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[JsValueMapper[T]]
    def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]]
    override def toString: String = s"${getClass.getSimpleName}[TODO]" // TODO

  private def summonJsValueMapper(using Quotes)(tpe: quotes.reflect.TypeRepr, deps:Map[quotes.reflect.TypeRepr, TermBuilder]): Option[Expr[JsValueMapper[_]]] =
    tpe.asType match
      case '[t] => summonJsValueMapper[t](using quotes)(deps)

  private def summonJsValueMapper[t: Type](using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Option[Expr[JsValueMapper[t]]] =
    import quotes.reflect.*
    if deps contains TypeRepr.of[t] then
      Some(deps(TypeRepr.of[t]).make.asExprOf[JsValueMapper[t]])
    else
      try
        ADTMappingMacro.NO_EXPAND_ADT.set(true); ADTMappingMacro.NOT_EXPAND_ADTS.set(0)
        val found = Expr.summon[JsValueMapper[t]]
        if ADTMappingMacro.NOT_EXPAND_ADTS.get().nn > 0 then
          None
        else found
      finally
        ADTMappingMacro.NO_EXPAND_ADT.set(false)

  private class ProductGenerator[T: Type] extends Generator[T]:

    override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      import quotes.reflect.*
      val defaultParams: Map[String, Expr[Any]] = extractDefaultParams[T]

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
                  if isPrimitive then '{ ${mapper}.toJson(${select}) }
                  else '{ if ${select} == null then JsNull else ${mapper}.toJson($select) }

                '{ (${ Expr(field.name) }, ${json}) }
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

  // TODO, add more collection here
  private class ListGenereator[T: Type] extends Generator[T]:

    override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      import quotes.reflect.*
      baseTpe.asInstanceOf[AppliedType].args(0).asType match
        case '[t] =>
          val ref = summonJsValueMapper[t](deps).get
          '{
            new JsValueMapper[List[t]]:
              def fromJson(js: JsValue): List[t] = js match
                case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toList
                case _ => ???

              def toJson(x: List[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
          }.asExprOf[JsValueMapper[T]]

  private class OrTypeGenerator[T: Type] extends Generator[T]:

    // X | Y | Z
    def elementTypes(using Quotes)(tpe: quotes.reflect.TypeRepr) : List[quotes.reflect.TypeRepr] =
      tpe match
        case quotes.reflect.OrType(l, r) => elementTypes(l) ++ elementTypes(r)
        case _ => List(tpe)

    override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      import quotes.reflect.*
      val elemenTypeReprs: List[TypeRepr] = elementTypes(TypeRepr.of[T])
      val usingTag = false  // TODO 是否使用 _tag_ 字段来区分类型
      val tags: List[String] = Nil // TODO

      if usingTag then
        generateWithTags(elemenTypeReprs, tags, deps)
      else
        generateWithoutTags(elemenTypeReprs, deps)

    private def generateWithTags(using Quotes)(elementTypes: List[quotes.reflect.TypeRepr], tags: List[String], deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      ???

    private def generateWithoutTags(using Quotes)(elementTypes: List[quotes.reflect.TypeRepr], deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      import quotes.reflect.*

      // the following code not works when inline it, so extract to a method
      def make[t:Type](mapper: Expr[JsValueMapper[t]], obj: Expr[t]): Expr[JsValue]=
        '{ ${mapper}.toJson(${obj}) }

      def toJsonImpl(obj: Expr[T]): Expr[JsValue] =
        val cases: List[CaseDef] = elementTypes.map(_.asType).map:
          case '[t] if TypeRepr.of[t] =:= TypeRepr.of[Null] =>
            CaseDef( Literal(NullConstant()), None, '{ JsNull }.asTerm )
          case '[t] =>
            val dep: Expr[JsValueMapper[t]] = summonJsValueMapper[t](deps).get
            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[t], Flags.EmptyFlags, Symbol.noSymbol)
            val refSym: Expr[t] = Ref(sym).asExprOf[t]
            val bindPattern = Typed(Wildcard(), TypeTree.of[t])
            val pattern = Bind( sym, bindPattern )
            val body =  make[t](dep, refSym).asTerm
            CaseDef( pattern, None, body)

        Match( obj.asTerm, cases).asExprOf[JsValue]

      // JsNull -> null or None
      // JsString, JsBoolean, JsNumber -> string or Some(string), etc.
      // JsArray -> Array or List
      // JsObject -> Map or case class
      def fromNull(): Expr[T] =
        if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Null] ) then
          '{ null.asInstanceOf[T] }
        else if elementTypes.exists( tpe => tpe <:< TypeRepr.of[Option[?]] ) then
          '{ None.asInstanceOf[T] }
        else '{ throw new RuntimeException("No null value allowed") }

      def fromString(value: Expr[String]): Expr[T] =
        if elementTypes.exists( tpe => tpe =:= TypeRepr.of[String] ) then
          '{ ${value}.asInstanceOf[T]  }
        else '{ throw new RuntimeException("No string value allowed") }

      def fromLong(value: Expr[Long]): Expr[T] =
        if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Long] ) then
          '{ ${value}.asInstanceOf[T] }
        else if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Int]) then
          '{ ${value}.toInt.asInstanceOf[T]  }
        else '{ throw new RuntimeException("No long value allowed") }

      def fromDouble(value: Expr[Double]): Expr[T] =
        if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Double] ) then
          '{ ${value}.asInstanceOf[T] }
        else if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Float]) then
          '{ ${value}.toFloat.asInstanceOf[T] }
        else '{ throw new RuntimeException("No double value allowed") }

      def fromBoolean(value: Expr[Boolean]): Expr[T] =
        if elementTypes.exists( tpe => tpe =:= TypeRepr.of[Boolean] ) then
          '{ ${value}.asInstanceOf[T]  }
        else '{ throw new RuntimeException("No boolean value allowed") }

      def fromArray(js: Expr[JsArray]): Expr[T] = '{ throw new RuntimeException("fromArray is not implemented yet") }
      def fromObject(js: Expr[JsObject]): Expr[T] = '{ throw new RuntimeException("fromObject is not implmented yet") }


      '{
        new JsValueMapper[T]:
          override def fromJson(js: JsValue): T = // ${fromJsonImpl('{js}) }
            js match
              case JsNull => ${fromNull()}
              case JsString(s) => ${fromString('s)}
              case JsNumber(l: Long) => ${fromLong('l)}
              case JsNumber(d: Double) => ${fromDouble('d)}
              case JsBoolean(b) => ${fromBoolean('b)}
              case s: JsArray => ${fromArray('s)}
              case s: JsObject => ${fromObject('s)}

          override def toJson(x: T): JsValue = ${toJsonImpl( '{x} )}
      }

  private class SumGenerator[T: Type] extends Generator[T]:

    override def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      Expr.summon[Mirror.Of[T]].get match
        case '{ $m: Mirror.SumOf[T] {type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames} } =>
          generate[elemTypes, elemNames](deps)

    private def generate[elemTypes: Type, elemNames: Type](using Quotes)(deps: Map[quotes.reflect.TypeRepr, TermBuilder]): Expr[JsValueMapper[T]] =
      import quotes.reflect.*
      val names = extractNames[elemNames]
      val typeReprs = extractTypeReprs[elemTypes]

      val nameTypes: List[(String, TypeRepr)] = names.zip(typeReprs)

      /**
       * <pre>
       * def fromJson(json: JsValue): Color = json match
       * case JsString("Red") => Red
       * ...
       * case x:JsObject if x.fields("_kind") == "Mixed" => summon[Mixed].fromJson(json)
       * </pre>
       */
      def fromJsonImpl(js: Expr[JsValue]): Expr[T] =
        val cases = nameTypes.map { arg =>
          val name: String = arg._1
          val tpe: TypeRepr = arg._2
          if (tpe.termSymbol != Symbol.noSymbol) { // case Red of single value
            val selector = Ref(Symbol.requiredModule("wjson.JsValue.JsString"))
            val tree = Unapply(Select.unique(selector, "unapply"), Nil, List(Literal(StringConstant(name))))
            val tpt = Inferred(TypeRepr.of[JsString])
            val pattern = TypedOrTest(tree, tpt)
            CaseDef( pattern, None, Ref.term(tpe.asInstanceOf[TermRef]) ) // case JsString("Red") => Red
          }
          else { // case Mixed(rgb) of a case class
            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[JsValue.JsObject], Flags.EmptyFlags, Symbol.noSymbol)
            val bindPattern = Typed(Wildcard(), TypeTree.of[JsValue.JsObject])
            val pattern = Bind(sym, bindPattern)
            val xExpr = Ref(sym).asExprOf[JsValue.JsObject]
            val kind: Expr[String] = Expr(name)
            val guard = '{ ${ xExpr }.field("_kind") == JsString(${ kind }) }

            val mapper = summonJsValueMapper(using quotes)(tpe, deps).get
            val body = '{ ${mapper}.fromJson(${xExpr}) }

            CaseDef(pattern, Some(guard.asTerm), body.asTerm) // case _x: JsObject if _x.fields("_kind") == JsString("Mixed") =>
          }
        }
        val cases2 = cases :+ CaseDef(Wildcard(), None, '{ throw new Exception("no _kind field") }.asTerm) // case _ => throw new Exception("no _kind field")

        val expr2 = Match(js.asTerm, cases2)
        expr2.asExprOf[T]

      /**
       * <pre>
       * def toJson(t: Color): JsValue = t match
       * case Red => JsString("Red")
       * ...
       * case x: Mixed => summon[Mixed].toJson(x)
       * </pre>
       */
      def toJsonImpl(t: Expr[T]): Expr[JsValue] =
        val cases = nameTypes.map { arg =>
          val name = arg._1
          val typ: TypeRepr = arg._2
          val nameExpr = Expr(name)
          if (typ.termSymbol != Symbol.noSymbol) { // case Red => JsString("Red")
            val pattern = Ref.term(typ.asInstanceOf[TermRef])
            val body = '{ JsString(${ nameExpr }) }
            CaseDef(pattern, None, body.asTerm)
          }
          else { // case _x: Mixed => JsObject( summon[Mixed].toJson(_x).fields + ("_kind" -> JsString("Mixed")) )
            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", typ, Flags.EmptyFlags, Symbol.noSymbol)
            val typeTree = TypeTree.of(using typ.asType)
            val bindPattern = Typed(Wildcard(), typeTree)
            val pattern = Bind(sym, bindPattern)

            val body = typ.asType match
              case '[t] =>
                val xExpr = Ref(sym).asExprOf[t]
                val mapper = summonJsValueMapper[t](deps).get
                '{ JsObject("_kind" -> JsString(${ nameExpr })) ++ ${mapper}.toJson(${xExpr}).asInstanceOf[JsObject] }
            CaseDef(pattern, None, body.asTerm)
          }
        }
        val expr = Match(t.asTerm, cases)
        expr.asExprOf[JsValue]

      val expr = '{
            new JsValueMapper[T]:
              def fromJson(js: JsValue): T = ${ fromJsonImpl('{ js }) }

              def toJson(t: T): JsValue = ${ toJsonImpl('{ t }) }
          }
      expr.asExprOf[JsValueMapper[T]]


  private type GeneratorMap = Map[q.reflect.TypeRepr, Generator[?] ]// Map[q.reflect.TypeRepr, Generator[?]]

  private def visit[T: Type](acc:  GeneratorMap) : GeneratorMap =
    given Quotes = q
    import q.reflect.*

    def visitList[T: Type](value: GeneratorMap): GeneratorMap =
      val generator = ListGenereator[T]()
      val acc2 = value + ( TypeRepr.of[T] -> generator)
      TypeRepr.of[T].asInstanceOf[AppliedType].args.foldLeft(acc2) {
        case (acc, arg) => arg.asType match
          case '[t] => visit[t](acc)
      }

    def visitADT[T: Type](value: GeneratorMap): GeneratorMap =
      Expr.summon[Mirror.Of[T]] match
        case Some('{ $m: Mirror.ProductOf[T] }) => visitProduct[T](acc)
        case Some('{ $m: Mirror.SumOf[T] {
          type MirroredElemTypes = elemTypes
          type MirroredElemLabels = elemLabels
        } }) => visitSum[T, elemTypes, elemLabels](acc)
        case None =>
            throw new NotImplementedError("Not a Product or Sum or OrType:" + TypeRepr.of[T].show)

    def visitInside[T: Type](acc: GeneratorMap): GeneratorMap =
      TypeRepr.of[T] match
        case tpe if tpe <:< TypeRepr.of[List[_]] => visitList[T](acc)
        case OrType(l, r) => visitOrType[T](acc)
        case _ =>visitADT[T](acc)

    def visitProduct[T: Type](acc: GeneratorMap): GeneratorMap =
      val generator = ProductGenerator[T]() // (TypeRepr.of[T], GeneratorKind.GenProduct)
      val acc2 = acc + (TypeRepr.of[T] -> generator)

      TypeTree.of[T].symbol.caseFields.foldLeft(acc2):  (acc, field) =>
        field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
          case '[t] => visit[t](acc)

    def visitSum[T: Type, elemTypes:Type, elemNames:Type](result: GeneratorMap): GeneratorMap =

      val generator = SumGenerator[T]() // Generator(TypeRepr.of[T], GeneratorKind.GenSum)
      val r2 = result + (TypeRepr.of[T] -> generator)

      val tpes = extractTypeReprs[elemTypes]
      tpes.foldLeft(r2): (acc, tpe) =>
        val isParameterizedCase = tpe.termSymbol == Symbol.noSymbol
        if isParameterizedCase then
          tpe.asType match
            case '[t] => visit[t](acc)
        else acc  // simple case falls into the SumGenerator process

    def flatternOrTypeElements(tpe: TypeRepr): List[TypeRepr] =
      tpe match
        case OrType(left, right) => flatternOrTypeElements(left) ++ flatternOrTypeElements(right)
        case _ => List(tpe)

    def visitOrType[T: Type](acc: GeneratorMap): GeneratorMap =
      val generator = OrTypeGenerator[T]() // Generator(TypeRepr.of[T], GeneratorKind.GenOrType)
      val r2 = acc + (TypeRepr.of[T] -> generator)

      val flatterned: List[q.reflect.TypeRepr] = flatternOrTypeElements(TypeRepr.of[T])

      flatterned.foldLeft(r2) { (acc, tpe) =>
        tpe.asType match
          case '[t] => visit[t](acc)
      }

    if TypeRepr.of[T] =:= TypeRepr.of[Null] then acc
    else if acc.isEmpty then                             // this is the root type, dont summon self, on derived case, it maybe has a non-initialized value
      visitInside[T](acc)
    else if acc.contains(TypeRepr.of[T]) then acc   // already visited  // TDO Type[?] is not a good key
    else                                            // a new Type, first summon it, if success, skp it, otherwise, visit it
      ADTMappingMacro.NOT_EXPAND_ADTS.set(0)
      val found = Expr.summon[JsValueMapper[T]]   // here we will not expand ADT, but increase the counter
      if ADTMappingMacro.NOT_EXPAND_ADTS.get().nn > 0  then  // the type contains ADT need to expand
        visitInside[T](acc)
      else  found  match
        case Some(_) => acc
        case None => visitInside[T](acc)