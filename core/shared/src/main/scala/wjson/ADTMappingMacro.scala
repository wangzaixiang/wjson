package wjson

import wjson.ADTMappingMacro.{PRINT_GENERATED_CODE}

import scala.deriving.*
import scala.quoted.*
import wjson.{*, given}

import scala.annotation.experimental


/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object ADTMappingMacro:

  inline def genADT[T: deriving.Mirror.Of]: JsValueMapper[T] = ${ genADTImpl[T] }

  private def genADTImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    new ADTMappingMacro(using quotes).genADTImpl[T]

  def caseFieldGet[T: JsValueMapper](js: JsObject, name: String): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] if x.value ne JsNull => summon[JsValueMapper[T]].fromJson(x.value)
      case _ => throw new Exception("Expected field " + name + " not exists in JSON")

  def caseFieldGet[T: JsValueMapper](js: JsObject, name: String, default: T): T =
    js.fieldOpt(name) match
      case x: Some[JsValue] => if x.value eq JsNull then default else summon[JsValueMapper[T]].fromJson(x.value)
      case _ => default

  private val NO_EXPAND_ADT = new ThreadLocal[Boolean]:
    override def initialValue(): Boolean = false

  // use ThreadLocal to trace the recursive calls
  private val NOT_EXPAND_ADTS = new ThreadLocal[ Int ]():
    override def initialValue(): Int = 0

  val PRINT_GENERATED_CODE = java.lang.Boolean.getBoolean("wjson.printGeneratedCode")

  var count = 0

class ADTMappingMacro(using quotes: Quotes):

  import quotes.reflect.*

  private def extractTypeRepres[T: Type]: List[quotes.reflect.TypeRepr] =
    Type.of[T] match
      case '[t *: ts] => quotes.reflect.TypeRepr.of[t] :: extractTypeRepres[ts]
      case '[EmptyTuple] => Nil

  private def extractNames[T: Type]: List[String] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: ts] => TypeRepr.of[t] match
        case ConstantType(StringConstant(name)) =>
          name :: extractNames[ts]
        case _ => throw new AssertionError("Expected a String constant type")
      case '[EmptyTuple] => Nil

  private def genADTImpl[T: Type]: Expr[JsValueMapper[T]] =

    if ADTMappingMacro.NO_EXPAND_ADT.get() then
      ADTMappingMacro.NOT_EXPAND_ADTS.set(ADTMappingMacro.NOT_EXPAND_ADTS.get() + 1)
      '{ ??? }
    else
      val dependencies =
        try
          ADTMappingMacro.NO_EXPAND_ADT.set(true)
          Expr.summon[Mirror.Of[T]].get match
            case '{ $m: Mirror.ProductOf[T] } =>
//              val generator = ProductGenerator[T]() // Generator( TypeRepr.of[T], GeneratorKind.GenProduct )
                visit[T](Map.empty )
//              visit[T](Map(generator.baseTpt -> generator) )
            case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames } } =>
              val generator = SumGenerator[T]() // Generator( TypeRepr.of[T], GeneratorKind.GenSum )
              visit[T](Map.empty )
              // visit[T](Map(generator.baseTpt -> generator) )
            case _ => throw new AssertionError("Expected a Product or a Sum")
        finally
          ADTMappingMacro.NO_EXPAND_ADT.set(false)
      //if PRINT_GENERATED_CODE then
      println("!!! dependencies: " + dependencies)
      genMultiMapperBlock[T](dependencies)

  private def extractDefaultParams[T: Type]: Map[String, Expr[Any]] =
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

  private def genMultiMapperBlock[T: Type](needGenTypes: GeneratorMap): Expr[JsValueMapper[T]] =
    var i = 0  // variable name counter
    val valSyms: Map[TypeRepr, Symbol] = needGenTypes map: (tpt, generator) =>
      i += 1
      val sym = Symbol.newVal( Symbol.spliceOwner, s"mapper_${i}",  generator.mapperTpt, Flags.Lazy, Symbol.noSymbol )
      (tpt, sym )

    val refs: Map[TypeRepr, ()=>Term] = valSyms map : (tpt, sym) =>
      (tpt, ()=> Ref(sym))

    val valDefs: Map[TypeRepr, ValDef] = needGenTypes map: (tpt, generator) =>
      val sym = valSyms(tpt)

      val mapper = generator.generate(refs).asTerm
      val valdef = ValDef( sym, Some(mapper) )
      (tpt, valdef)

    val term  = Block(valDefs.values.toList, refs(TypeRepr.of[T]).apply() )

    //if ADTMappingMacro.PRINT_GENERATED_CODE then
    println("generated JsValueMapper[" + TypeRepr.of[T].show(using Printer.TypeReprAnsiCode) + "] = "
        + term.show(using Printer.TreeAnsiCode))

    term.asExpr.asInstanceOf[Expr[JsValueMapper[T]]]
//
//  private def genMultiMapperBlock1[T: Type](needGenTypes: GeneratorMap): Expr[JsValueMapper[T]] =
//    var i = 0 // variable name counter
//    val count = needGenTypes.size
//
//    val valSyms: Map[TypeRepr, (Symbol, Symbol)] = needGenTypes map : (tpt, generator) =>
//      i += 1
//      val sym = Symbol.newVal( Symbol.spliceOwner, s"_mapper_${i}", generator.mapperTpt, Flags.Mutable, Symbol.noSymbol)
//      val methodType = MethodType(Nil)( _ => Nil, _ => generator.mapperTpt )
//      val defSym = Symbol.newMethod( Symbol.spliceOwner, s"mapper_$i", methodType, Flags.Private, Symbol.noSymbol)
//
//      (tpt, (sym, defSym))
//
//    val refs: Map[TypeRepr, ()=>Term] = needGenTypes map : (tpt, generator) =>
//      val defSym = valSyms(tpt)._2
//      val call = () => Apply( Ref(defSym), Nil)
//      (tpt, call)
//
//    val varDefs: Map[TypeRepr, ValDef] = needGenTypes map : (tpt, generator) =>
//      val varSym = valSyms(tpt)._1
//      val varDef = ValDef( varSym, Some( Literal(NullConstant()) ) )
//      (tpt, varDef)
//
//    val defDefs: Map[TypeRepr, DefDef] = needGenTypes map: (tpt, generator) =>
//      val defSym: Symbol = valSyms(tpt)._2
//
//      val varRef: Ref = Ref(valSyms(tpt)._1)
//      val if1 = If( Apply( Select.unique(varRef, "=="), List(Literal(NullConstant())) ),
//        Assign(varRef, generator.generate(refs).asTerm),
//        Literal(UnitConstant() ) )
//
//      val block = Block( List(if1), varRef)
//      val defdef = DefDef(defSym, _ => Some(block))
//      (tpt, defdef)
//
//    val term = Block( Nil ++ varDefs.values ++ defDefs.values,
//      refs(TypeRepr.of[T]).apply() )
//
//    //if ADTMappingMacro.PRINT_GENERATED_CODE then
//    println("generated JsValueMapper[" + TypeRepr.of[T].show(using Printer.TypeReprAnsiCode) + "] = "
//      + term.show(using Printer.TreeAnsiCode))
//
//    term.asExprOf[JsValueMapper[T]]
////

//  private def genMultiMapperBlock[T: Type](needGenTypes: GeneratorMap): Expr[JsValueMapper[T]] =
//    var i = 0 // variable name counter
//    val count = needGenTypes.size
//
//    val valSyms: Map[TypeRepr, (Symbol, Symbol)] = needGenTypes map : (tpt, generator) =>
//      i += 1
//      val sym = Symbol.newVal(Symbol.spliceOwner, s"_mapper_${i}", generator.mapperTpt, Flags.Mutable, Symbol.noSymbol)
//      val methodType = MethodType(Nil)(_ => Nil, _ => generator.mapperTpt)
//      val defSym = Symbol.newMethod(Symbol.spliceOwner, s"mapper_$i", methodType, Flags.Private, Symbol.noSymbol)
//
//      (tpt, (sym, defSym))
//
//    val refs: Map[TypeRepr, () => Term] = needGenTypes map : (tpt, generator) =>
//      val valSym = valSyms(tpt)._1
//      val call = () => Ref(valSym)
//      (tpt, call)
//
//    val varDefs: Map[TypeRepr, ValDef] = needGenTypes map : (tpt, generator) =>
//      val varSym = valSyms(tpt)._1
//      val varDef = ValDef(varSym, Some(Literal(NullConstant())))
//      (tpt, varDef)
//
//    val defDefs: Map[TypeRepr, DefDef] = needGenTypes map : (tpt, generator) =>
//      val defSym: Symbol = valSyms(tpt)._2
//      val body = Block(Nil, generator.generate(refs).asTerm )
//      val defdef = DefDef(defSym, _ => Some(body))
//      (tpt, defdef)
//
//    val assigns = needGenTypes map: (tpt, generator) =>
//      val valSym = valSyms(tpt)._1
//      val defSym = valSyms(tpt)._2
//
//      (tpt, Assign( Ref(valSym), Apply(Ref(defSym), Nil)))
//
//    val term = Block(Nil ++ varDefs.values ++ defDefs.values ++ assigns.values,
//      refs(TypeRepr.of[T]).apply() )
//
//    //if ADTMappingMacro.PRINT_GENERATED_CODE then
//    println("generated JsValueMapper[" + TypeRepr.of[T].show(using Printer.TypeReprAnsiCode) + "] = "
//      + term.show(using Printer.TreeAnsiCode))
//
//    term.asExprOf[JsValueMapper[T]]
//  //


  private trait Generator[T: Type]:
    def baseTpt: TypeRepr = TypeRepr.of[T]
    def mapperTpt: TypeRepr = TypeRepr.of[JsValueMapper[T]]
    // deps: t -> ref( JsValueMapper[t] )
    def generate(deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]]
    override def toString: String = s"${getClass.getSimpleName}[${TypeRepr.of[T].show(using Printer.TypeReprAnsiCode)}]"


  private class ProductGenerator[T: Type] extends Generator[T]:

     def generate(deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]] =

      val defaultParams: Map[String, Expr[Any]] = extractDefaultParams[T]

      def buildBeanFrom(jso: Expr[JsObject]): Expr[T] =
        val tpeSym = TypeTree.of[T].symbol
        val terms: List[Term] = tpeSym.caseFields.map(field => getField(jso, field))
        val constructor = tpeSym.primaryConstructor

        ValDef.let(Symbol.spliceOwner, terms) { refs =>
          Apply(Select(New(TypeTree.of[T]), constructor), refs)
        }.asExpr.asInstanceOf[Expr[T]]

      def buildJsVal(value: Expr[T]): Expr[JsValue] =
        val tpeSym = TypeTree.of[T].symbol
        val terms: List[Expr[(String, JsValue)]] = tpeSym.caseFields.map(field => getFieldAsKV(value, field))
        val asSeq = Expr.ofSeq(terms)
        '{ JsObject((${ asSeq }).filter(_._2 != JsNull): _*) }

      // '{ new CaseField[t](field, default).apply(jso) }'
      def getField(jso: Expr[JsObject], field: Symbol): Term =
        val name = field.name
        val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
          case '[t] =>
            val summonValue: Option[Expr[JsValueMapper[t]]] =
              val twiden = TypeRepr.of[t].widen
              if deps contains twiden then
                val dep = deps(twiden).apply()
                Some(dep.asExprOf[JsValueMapper[t]])
              else Expr.summon[JsValueMapper[t]]

            val isOption = TypeRepr.of[t] <:< TypeRepr.of[Option[?]]

            summonValue match
              case Some(mapper) =>
                import ADTMappingMacro.*
                defaultParams.get(name) match
                  case Some(_default) =>
                    '{ caseFieldGet[t]($jso, ${ Expr(name) }, ${ _default.asInstanceOf[Expr[t]] })(using $mapper) }
                  case None if isOption == false =>
                    '{ caseFieldGet[t]($jso, ${ Expr(name) })(using $mapper) }
                  case None =>
                    '{ caseFieldGet[t]($jso, ${ Expr(name) }, None.asInstanceOf[t])(using $mapper) }

              case None =>
                report.error(s"No JsValueMapper found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
                '{ ??? }

        expr.asTerm

      // '{ (field.name, summon[JsValueMapper[field.type].toJson(value)]) }
      def getFieldAsKV(value: Expr[T], field: Symbol): Expr[(String, JsValue)] =
        field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
          case '[t] =>
            val isPrimitive = TypeRepr.of[t] <:< TypeRepr.of[AnyVal]
            val summonValue =
              val twiden = TypeRepr.of[t].widen
              if deps contains twiden then
                val dep = deps(twiden).apply()
                Some(dep.asExprOf[JsValueMapper[t]])
              else Expr.summon[JsValueMapper[t]]

            summonValue match
              case Some(mapper) =>
                val select = Select(value.asTerm, field).asExpr.asInstanceOf[Expr[t]]
                val json =
                  if isPrimitive then '{ $mapper.toJson($select) }
                  else '{ if $select == null then JsNull else $mapper.toJson($select) }

                '{ (${ Expr(field.name) }, $json) }
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

  private class ListGenereator[T: Type] extends Generator[T]:

    override def generate(deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]] =
      baseTpt.asInstanceOf[AppliedType].args(0).asType match
        case '[t] =>
          assert( deps contains TypeRepr.of[t])
          val dep = deps(TypeRepr.of[t]).apply()
          val ref = dep.asExprOf[JsValueMapper[t]]
          '{
            new JsValueMapper[List[t]]:
              def fromJson(js: JsValue): List[t] = js match
                case x: JsArray => x.elements.map(x => ${ ref }.fromJson(x)).toList
                case _ => ???

              def toJson(x: List[t]): JsValue = JsArray(x.map(x => ${ ref }.toJson(x)))
          }.asExprOf[JsValueMapper[T]]

  private class OrTypeGenerator[T: Type] extends Generator[T]:
    override def generate(deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]] = ???

  private class SumGenerator[T: Type] extends Generator[T]:

    override def generate(deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]] =
      Expr.summon[Mirror.Of[T]].get match
        case '{ $m: Mirror.SumOf[T] {type MirroredElemTypes = elemTypes; type MirroredElemLabels = elemNames} } =>
          generate[elemTypes, elemNames](deps)

    private def generate[elemTypes: Type, elemNames: Type](deps: Map[TypeRepr, ()=>Term]): Expr[JsValueMapper[T]] =
      val names = extractNames[elemNames]
      val typeReprs = extractTypeRepres[elemTypes]

      val nameTypes: List[(String, TypeRepr)] = names.zip(typeReprs)

//      val caseMappers: Map[String,Term] = nameTypes.filter { case (_, tpe) => tpe.termSymbol == Symbol.noSymbol }
//        .map { case (name, tpe) => (name, deps(tpe).apply() ) }.toMap

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
            // The following code has bug on a this-enclosing enum type( class level or method level)
            // TODO need to report the bug
            CaseDef(pattern, None, Ref(tpe.termSymbol)) // case JsString("Red") => Red
          }
          else { // case Mixed(rgb) of a case class
            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", TypeRepr.of[JsValue.JsObject], Flags.EmptyFlags, Symbol.noSymbol)
            val bindPattern = Typed(Wildcard(), TypeTree.of[JsValue.JsObject])
            val pattern = Bind(sym, bindPattern)
            val xExpr = Ref(sym).asExprOf[JsValue.JsObject]
            val kind: Expr[String] = Expr(name)
            val guard = '{ ${ xExpr }.field("_kind") == JsString(${ kind }) }

            val mapper = deps(tpe).apply().asExprOf[JsValueMapper[_]]
            val body = '{ $mapper.fromJson($xExpr) }

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
            val pattern = Ref(typ.termSymbol)
            val body = '{ JsString(${ nameExpr }) }
            CaseDef(pattern, None, body.asTerm)
          }
          else { // case _x: Mixed => JsObject( summon[Mixed].toJson(_x).fields + ("_kind" -> JsString("Mixed")) )
            val sym = Symbol.newVal(Symbol.spliceOwner, "_x", typ, Flags.EmptyFlags, Symbol.noSymbol)
            val typeTree = TypeTree.of(using typ.asType)
            val bindPattern = Typed(Wildcard(), typeTree)
            val pattern = Bind(sym, bindPattern)
            val xExpr: Expr[Any] = Ref(sym).asExprOf[Any]

            val mapper = deps(typ).apply().asExprOf[JsValueMapper[_]].asInstanceOf[Expr[JsValueMapper[Any]]]
            val body = '{ JsObject("_kind" -> JsString(${ nameExpr })) ++ $mapper.toJson($xExpr).asInstanceOf[JsObject] }

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
      // println("expr = " + expr.show)
      expr.asExprOf[JsValueMapper[T]]


  private type GeneratorMap = Map[TypeRepr, Generator[?]]

  private def visit[T: Type](acc:  GeneratorMap) : GeneratorMap =

    def visitList[T: Type](value: GeneratorMap): GeneratorMap =
      val generator = ListGenereator[T]()
      val acc2 = value + ( generator.baseTpt -> generator)
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
          TypeRepr.of[T] match
            case OrType(left, right) => visitOrType[T](left, right, acc)
            case _ => throw new NotImplementedError("Not a Product or Sum or OrType:" + TypeRepr.of[T].show)
        case _ =>
          ???

    def visitInside[T: Type](value: GeneratorMap): GeneratorMap =
      if TypeRepr.of[T] <:< TypeRepr.of[List[_]] then  // TODO: more collections
        visitList[T](value)
      else
        visitADT[T](value)

    def visitProduct[T: Type](acc: GeneratorMap): GeneratorMap =
      val generator = ProductGenerator[T]() // (TypeRepr.of[T], GeneratorKind.GenProduct)
      val acc2 = acc + (generator.baseTpt -> generator)

      TypeTree.of[T].symbol.caseFields.foldLeft(acc2):  (acc, field) =>
        field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
          case '[t] => visit[t](acc)

    def visitSum[T: Type, elemTypes:Type, elemNames:Type](result: GeneratorMap): GeneratorMap =

      val generator = SumGenerator[T]() // Generator(TypeRepr.of[T], GeneratorKind.GenSum)
      val r2 = result + (generator.baseTpt -> generator)

      val tpes = extractTypeRepres[elemTypes]
      tpes.foldLeft(r2): (acc, tpt) =>
        val isParameterizedCase = tpt.termSymbol == Symbol.noSymbol
        if isParameterizedCase then
          tpt.asType match
            case '[t] => visit[t](acc)
        else acc  // simple case falls into the SumGenerator process

    def visitOrType[T: Type](left: TypeRepr, right: TypeRepr, result: GeneratorMap): GeneratorMap =
      val generator = OrTypeGenerator[T]() // Generator(TypeRepr.of[T], GeneratorKind.GenOrType)
      val r2 = result + (generator.baseTpt -> generator)

      val r_left = left.asType match
        case '[t] => visit[t](r2)
      val r_right = right.asType match
        case '[t] => visit[t](r_left)
      r_right

    if acc.size == 0 then // this is the root type
      visitInside[T](acc)  // dont summon self, on derived case, it maybe has a non-initialized value
    else if acc.contains(TypeRepr.of[T]) then acc  // already visited
    else // a new Type, first summon it, if success, skp it, otherwise, visit it
      ADTMappingMacro.NOT_EXPAND_ADTS.set(0)
      val found = Expr.summon[JsValueMapper[T]]  // here we will not expand ADT, but increase the counter
      if ADTMappingMacro.NOT_EXPAND_ADTS.get() > 0  then  // the type contains ADT need to expand
        visitInside[T](acc)
      else  found  match
        case Some(_) => acc
        case None => visitInside[T](acc)