package wjson.macros

import scala.quoted.*
import scala.deriving.*
import wjson.{*, given}

import scala.Symbol as _

/**
 * Macro to generate a JsValueMapper for a given case class.
 */
object ADTMappingMacro:

  def genADTImpl[T: Type](using Quotes): Expr[JsValueMapper[T]] =
    new ADTMappingMacro(quotes).genADTImpl[T]

  private val NO_EXPAND_ADT = new ThreadLocal[Boolean]:
    override def initialValue(): Boolean = false

  // use ThreadLocal to trace the recursive calls
  private val NOT_EXPAND_ADTS = new ThreadLocal[Int]():
    override def initialValue(): Int = 0

  /**
   * to enable macro debug, runs like `sbt -Dwjson.printMacroCode=true compile`
   */
  private val PRINT_MACRO_CODE: Boolean = java.lang.Boolean.getBoolean("wjson.printMacroCode")

  def extractElemTypes[T: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    Type.of[T] match
      case '[t *: ts] => quotes.reflect.TypeRepr.of[t] :: extractElemTypes[ts]
      case '[EmptyTuple] => Nil

  def extractElemLabels[T: Type](using Quotes): List[String] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: ts] => TypeRepr.of[t] match
        case ConstantType(StringConstant(name)) =>
          name :: extractElemLabels[ts]
        case _ => throw new AssertionError("Expected a String constant type")
      case '[EmptyTuple] => Nil

  def extractDefaultCaseParams[T: Type](using Quotes): Map[String, Expr[Any]] =
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

  trait Generator[T: Type]:
    def baseTpe(using quotes: Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[T]
    def mapperTpe(using quotes: Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[JsValueMapper[T]]

    /**
     * deps: Map[ TypeRepr: TypeRepr.of[t], Ref: JsValueMapper[t] ]
     */
    def generate(using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Expr[JsValueMapper[T]]

  def summonJsValueMapper(using Quotes)(tpe: quotes.reflect.TypeRepr, deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Option[Expr[JsValueMapper[_]]] =
    tpe.asType match
      case '[t] => summonJsValueMapper[t](using quotes)(deps)

  def summonJsValueMapper[t: Type](using Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Ref]): Option[Expr[JsValueMapper[t]]] =
    import quotes.reflect.*
    if deps contains TypeRepr.of[t] then
      Some(deps(TypeRepr.of[t]).asExprOf[JsValueMapper[t]])
    else
      try
        ADTMappingMacro.NO_EXPAND_ADT.set(true);
        ADTMappingMacro.NOT_EXPAND_ADTS.set(0)
        val found = Expr.summon[JsValueMapper[t]]
        if ADTMappingMacro.NOT_EXPAND_ADTS.get().nn > 0 then
          None
        else found
      finally
        ADTMappingMacro.NO_EXPAND_ADT.set(false)

class ADTMappingMacro(q: Quotes):

  import ADTMappingMacro.*

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


  /**
   * generate a block for each generator as a lazy val, so they can be referenced by each other
   */
  private def genMultiMapperBlock[T: Type](needGenTypes: GeneratorMap)(using Quotes): Expr[JsValueMapper[T]] =
    import quotes.reflect.*
    var i = 0  // variable name counter
    val valSyms: Map[TypeRepr, Symbol] = needGenTypes map: (tpe, generator) =>
      i += 1
      val sym = Symbol.newVal( Symbol.spliceOwner, s"mapper_${i}",  generator.mapperTpe, Flags.Lazy, Symbol.noSymbol )
      ( tpe.asInstanceOf[TypeRepr], sym )

    val refs: Map[TypeRepr, Ref] = valSyms map { (tpe, sym) => ( tpe, Ref(sym) ) }

    val valDefs: Map[TypeRepr, ValDef] = needGenTypes map: (tpe, generator) =>
      val sym: Symbol = valSyms( tpe.asInstanceOf[TypeRepr] )

      val nested = sym.asQuotes // the nested generator should be in the scope of the lazy val
      val refs2 = refs.asInstanceOf[Map[nested.reflect.TypeRepr, nested.reflect.Ref]]
      val mapper: Term = generator.generate(using nested)(refs2).asTerm
      val valDef = ValDef( sym, Some(mapper) )
      (tpe.asInstanceOf[TypeRepr], valDef)

    val term  = Block(valDefs.values.toList, refs(TypeRepr.of[T]) )

    if ADTMappingMacro.PRINT_MACRO_CODE then
      println("generated JsValueMapper[" + TypeRepr.of[T].show(using Printer.TypeReprAnsiCode) + "] = "
        + term.show(using Printer.TreeAnsiCode))

    term.asExpr.asInstanceOf[Expr[JsValueMapper[T]]]

  private type GeneratorMap = Map[q.reflect.TypeRepr, Generator[?] ]// Map[q.reflect.TypeRepr, Generator[?]]

  /**
   * recursive visit Type and it dependencies types and build the dependency map
   * for Product types, the dependencies is it's fields types.
   * for SUM types, the dependencies is it's elements types with recursive visit
   * for List[T], the dependencies is T with recursive visit
   * for U|V, the dependencies is U and V with recursive visit
   */
  private def visit[T: Type](acc:  GeneratorMap) : GeneratorMap =
    given Quotes = q
    import q.reflect.*

    def visitCollection[T: Type](value: GeneratorMap, generator: Generator[T]): GeneratorMap =
      val acc2 = value + ( TypeRepr.of[T] -> generator)
      TypeRepr.of[T].asInstanceOf[AppliedType].args.foldLeft(acc2):
        case (acc, arg) => arg.asType match
          case '[t] => visit[t](acc)

    def visitADT[T: Type](value: GeneratorMap): GeneratorMap =
      Expr.summon[Mirror.Of[T]] match
        case Some('{ $m: Mirror.ProductOf[T] }) => visitProduct[T](acc)
        case Some('{
          $m: Mirror.SumOf[T] {
            type MirroredElemTypes = elemTypes
            type MirroredElemLabels = elemLabels
          }
        }) => visitSum[T, elemTypes, elemLabels](acc)
        case Some(_) => ???
        case None =>
            throw new NotImplementedError("Not a Product or Sum or OrType:" + TypeRepr.of[T].show)

    def visitInside[T: Type](acc: GeneratorMap): GeneratorMap =
      TypeRepr.of[T] match
        case tpe if tpe <:< TypeRepr.of[List[_]] => visitCollection[T](acc, ListGenerator[T]() )
        case tpe if tpe <:< TypeRepr.of[Seq[_]] => visitCollection[T](acc, SeqGenerator[T]() )
        case tpe if tpe <:< TypeRepr.of[Vector[_]] => visitCollection[T](acc, VectorGenerator[T]() )
        case tpe if tpe <:< TypeRepr.of[Array[_]] => visitCollection[T](acc, ArrayGenerator[T]() )
        case tpe if tpe <:< TypeRepr.of[Set[_]] => visitCollection[T](acc, SetGenerator[T]() )
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

      val tpes = extractElemTypes[elemTypes]
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

      flatterned.foldLeft(r2): (acc, tpe) =>
        tpe.asType match
          case '[t] => visit[t](acc)

    if TypeRepr.of[T] =:= TypeRepr.of[Null] then acc
    else if acc.isEmpty then visitInside[T](acc)        // this is the root type, dont summon self, on derived case, it maybe has a non-initialized value
    else if acc.contains(TypeRepr.of[T]) then acc       // already visited  // TDO Type[?] is not a good key
    else                                                // a new Type, first summon it, if success, skp it, otherwise, visit it
      ADTMappingMacro.NOT_EXPAND_ADTS.set(0)
      val found = Expr.summon[JsValueMapper[T]]         // here we will not expand ADT, but increase the counter
      if ADTMappingMacro.NOT_EXPAND_ADTS.get().nn > 0  then  // the type contains ADT need to expand
        visitInside[T](acc)
      else  found  match
        case Some(_) => acc
        case None => visitInside[T](acc)

