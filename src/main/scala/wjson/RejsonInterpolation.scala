package wjson

import wjson.*
import wjson.JsPattern.Variable
import wjson.JsPattern._

import scala.jdk.CollectionConverters.*

class RejsonInterpolation(sc: StringContext) {
  
  object Placeholder {
    
    def apply(index: Int): JsString = JsString("placeholder_" + index + "_")
    
    def unapply(str: String): Option[Int] = {
      str match
        case str if str.startsWith("placeholder_") && str.endsWith("_") =>
          Some(str.substring(12, str.length - 1).toInt)
        case _ => None
    }
  }
  
  object InlineKey {
    def unapply(str: String): Option[Seq[String]] = {
      str match
        case str if str.contains("/") =>
          Some(str.split("/").toSeq.filter(_.nonEmpty))
        case _ => None
    }
  }
  
  def unapplyAsMap(input: JsValue): Option[Map[String, Any]] = ???
  
  def unapplySeq(input: JsValue): Option[Seq[Any]] = {
    
    val str = if sc.parts.size > 1 then
      sc.parts.head + sc.parts.tail.zipWithIndex.map { case (p, idx) => Placeholder(idx).value + p }.mkString
    else sc.parts.head
    
    val pattern = JsPatternParser.parseRejson(str)
    println(pattern)
    val results = collection.mutable.ArrayBuffer[Any]()
    Seq.range(0, sc.parts.length - 1).foreach { x => results += null }
    
    try {
      patternMatch(pattern, input, results)
      Some(results.toSeq)
    }
    catch {
      case ex: Throwable =>
        None
    }
  }
  
  private def parseType(value: JsValue, `type`: JsPattern) = `type` match
    case JsPattern.AnyVal(GroundType.STRING) => value.asInstanceOf[JsString].value
    case JsPattern.AnyVal(GroundType.BOOLEAN) => value.asInstanceOf[JsBoolean].value
    case JsPattern.AnyVal(GroundType.INTEGER) =>
      assert(value.asInstanceOf[JsNumber].value.isValidInt)
      value.asInstanceOf[JsNumber].value.toInt
    case JsPattern.AnyVal(GroundType.NUMBER) => value.asInstanceOf[JsNumber].value
    case JsPattern.StringPattern(_value) =>
      assert(value.isInstanceOf[JsString] && value.asInstanceOf[JsString].value == _value)
      _value
    case JsPattern.NumberPattern(_value) =>
      assert(value.isInstanceOf[JsNumber] && value.asInstanceOf[JsNumber].value == _value)
      _value
    case JsPattern.BoolPattern(_value) =>
      assert(value.isInstanceOf[JsBoolean] && value.asInstanceOf[JsBoolean].value == _value)
      _value
    case _ => value
  
  private def exactInlineKeys(unmerge: Map[String, Variable]): Map[String, Variable] = {
    unmerge
  }
  
  private def objPatternMatch(value: Map[String, Variable], input: JsValue,
                              results: collection.mutable.ArrayBuffer[Any]): Unit = {
    assert(input.isInstanceOf[JsObject])
    val inputObj = input.asInstanceOf[JsObject]
    
    val (_declared, others) = value.partition(_._2.pattern != AnyVals())
  
    val declared = exactInlineKeys(_declared)
  
    others.values.headOption match
      case Some(Variable(p, _)) =>
        val declaredKeys = declared.keys.toSet
        assert(declaredKeys -- inputObj.fields.keys.toSet == Set.empty)
        p match
          case Placeholder(index) =>
            results(index) = inputObj.fields.filterNot { case (k, _) => declaredKeys.contains(k) }.toMap
          case _ =>
      case _ =>
        assert(declared.keys.toSet == inputObj.fields.keys.toSet)
    
    declared.foreach {
      case (key, Variable(Placeholder(index), ptn: ArrPattern)) =>
        assert(inputObj.fields contains key)
        results(index) = parseType(inputObj.fields(key), ptn)
        patternMatch(Variable(null, ptn), inputObj.fields(key), results)
      
      case (key, Variable(Placeholder(index), ptn: ObjPattern)) =>
        assert(inputObj.fields contains key)
        results(index) = parseType(inputObj.fields(key), ptn)
        patternMatch(Variable(null, ptn), inputObj.fields(key), results)
      
      case (key, Variable(Placeholder(index), ptn)) =>
        assert(inputObj.fields contains key)
        results(index) = parseType(inputObj.fields(key), ptn)
      
      case (key, _value) =>
        assert(inputObj.fields contains key)
        patternMatch(_value, inputObj.fields(key), results)
    }
  }
  
  private def arrPatternMatch(value: List[Variable], input: JsValue,
                              results: collection.mutable.ArrayBuffer[Any]): Unit = {
    assert(input.isInstanceOf[JsArray])
    val inputArr = input.asInstanceOf[JsArray]
    
    if (value.exists(_.pattern == JsPattern.AnyVals())) {
      assert(input.asInstanceOf[JsArray].elements.size >= value.size - 1)
    } else {
      assert(input.asInstanceOf[JsArray].elements.size == value.size)
    }
    value.zipWithIndex.foreach {
      case (Variable(Placeholder(index), JsPattern.AnyVals()), idx) =>
        results(index) = inputArr.elements.takeRight(inputArr.elements.size - idx)
      case (Variable(Placeholder(index), ptn), y: Int) =>
        results(index) = parseType(inputArr.elements(y), ptn)
      case (x: Variable, y: Int) =>
        patternMatch(x, inputArr.elements.apply(y), results)
    }
  }
  
  private def patternMatch(pattern: Variable, input: JsValue,
                           results: collection.mutable.ArrayBuffer[Any]): Unit = pattern match
    
    case Variable(_, JsPattern.ObjPattern(value)) =>
      objPatternMatch(value, input, results)
    
    case Variable(_, JsPattern.ArrPattern(value)) =>
      arrPatternMatch(value, input, results)
    
    case Variable(_, JsPattern.AnyVal(GroundType.STRING)) =>
      assert(input.isInstanceOf[JsString])
    
    case Variable(_, JsPattern.AnyVal(GroundType.INTEGER)) =>
      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value.isValidInt)
    
    case Variable(_, JsPattern.AnyVal(GroundType.NUMBER)) =>
      assert(input.isInstanceOf[JsNumber])
    
    case Variable(_, JsPattern.AnyVal(GroundType.BOOLEAN)) =>
      assert(input.isInstanceOf[JsBoolean])
    
    case Variable(_, JsPattern.AnyVal(GroundType.ANY)) =>
      assert(true)
    
    case Variable(_, JsPattern.StringPattern(value)) =>
      assert(input.isInstanceOf[JsString] && input.asInstanceOf[JsString].value == value)
    
    case Variable(_, JsPattern.NumberPattern(value)) =>
      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value == value)
    
    case Variable(_, JsPattern.BoolPattern(value)) =>
      assert(input.isInstanceOf[JsBoolean] && input.asInstanceOf[JsBoolean].value == value)
    
    case Variable(_, JsPattern.TaggedString(tag, content)) =>
      // todo
      assert(true)
    case Variable(_, JsPattern.AnyVals()) =>
  
}