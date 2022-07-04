package wjson

import wjson.*
import wjson.JsPattern.Variable
import wjson.JsPattern.*
import wjson.JsValue.{JsArray, JsBoolean, JsNumber, JsObject}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/**
 * rejson is a pattern language for JSON
 */
class RejsonInterpolation(sc: StringContext):
  
  object Placeholder:
    def apply(index: Int): JsString = JsString("_placeholder_" + index + "_")
    def unapply(str: String): Option[String] =
      str match
        case str if str.startsWith("_placeholder_") && str.endsWith("_") =>
          Some(str.substring(13, str.length - 1))
        case _ => None

  object InlineKey:
    def unapply(str: String): Option[Seq[String]] =
      str match
        case str if str.contains("/") =>
          Some(str.split("/").toSeq.filter(_.nonEmpty))
        case _ => None

  
  def unapplyAsMap(input: JsValue): Option[Map[String, Any]] = {
    
    val str = if sc.parts.size > 1 then
      sc.parts.head + sc.parts.tail.zipWithIndex.map { case (p, idx) => Placeholder(idx).value + p }.mkString
    else sc.parts.head
    
    val pattern = JsPatternParser.parseRejson(str)
    
    val results = collection.mutable.Map[String, Any]()
    
    Seq.range(0, sc.parts.length - 1).foreach { x => results(x.toString) = null }
    
    try
      patternMatch(pattern, input, results)
      Some(results.toMap)
    catch
      case ex: Throwable =>
        None
  

  def unapplySeq(input: JsValue): Option[Seq[Any]] =
    unapplyAsMap(input).map(_.toList.filter(_._1.toIntOption.isDefined).sortBy(_._1.toInt).map(_._2))
  
  private def exactInlineKeys(unmerge: Map[String, Variable]): Map[String, Variable] =
    
    def getVariableName(names: Set[String], key: String): String =
      val varNames = names.filterNot(_ == null)
      if varNames.isEmpty then null else
        assert(varNames.size == 1, s"more than one variable name for: ${key}")
        varNames.head

    def deepMerge(key: String, left: Option[Variable], right: Option[Variable]): Variable =
      (left, right) match
        case (None, Some(r)) => r
        case (Some(l), None) => l
        case (Some(Variable(leftName, AnyVal(_))), Some(right@Variable(rightName, _))) =>
          Variable(getVariableName(Set(leftName, rightName), key), right.pattern)
        case (Some(left@Variable(leftName, _)), Some(Variable(rightName, AnyVal(_)))) =>
          Variable(getVariableName(Set(leftName, rightName), key), left.pattern)
        
        case (Some(Variable(leftName, ObjPattern(leftMap))), Some(Variable(rightName, ObjPattern(rightMap)))) =>
          val varName = getVariableName(Set(leftName, rightName), key)
          val value: Map[String, Variable] = Map((null, Variable(null, AnyVals()))) ++ leftMap.foldLeft(rightMap) {
            case (map, (k, v)) => map + (k -> deepMerge(k, map.get(k), Some(v)))
          }
          Variable(varName, ObjPattern(value))
        
        case (Some(Variable(leftName, ArrPattern(leftArr))), Some(Variable(rightName, ArrPattern(rightArr)))) =>
          val varName = getVariableName(Set(leftName, rightName), key)
          val (leftAnyVals, leftVariables) = leftArr.partition(_.pattern == AnyVals())
          val (rightAnyVals, rightVariables) = rightArr.partition(_.pattern == AnyVals())
          
          val anyVals = (leftAnyVals.map(_.name) ::: rightAnyVals.map(_.name))
            .distinct.map(Variable(_, AnyVals()))
            .filterNot(_.name == null)
          
          assert(anyVals.size <= 1, s"more than one variable name for: ${key}")
          
          val value = leftVariables.zipAll(rightVariables, null, null).zipWithIndex.map {
            case ((null, right), _) => right
            case ((left, null), _) => left
            case ((left, right), idx) => deepMerge(idx.toString, Some(left), Some(right))
          }
          
          Variable(varName, ArrPattern(value ::: anyVals))
        case (Some(Variable(leftName, left)), Some(Variable(rightName, right))) =>
          val varName = getVariableName(Set(leftName, rightName), key)
          assert(left == right, s"more than one check pattern exist for: ${key}")
          Variable(varName, left)
        
        case _ => throw new Exception("Cannot merge")

    
    def merge(toMerge: List[(String, Variable)]): Map[String, Variable] =
      val merged = collection.mutable.Map[String, Variable]()
      toMerge.foreach { case (key, value) =>
        merged.get(key) match
          case Some(variable) => merged(key) = deepMerge(key, Some(variable), Some(value))
          case _ => merged += key -> value
      }
      merged.toMap

    
    @tailrec
    def unfoldPaths(paths: List[String], thisMap: (String, Variable)): (String, Variable) =
      paths match
        case head :: tail =>
          unfoldPaths(tail, (head -> Variable(null, ObjPattern(Map((thisMap._1, thisMap._2), (null, Variable(null, AnyVals())))))))
        case _ => thisMap
    
    merge(unmerge.toList.map {
      case tuple@(InlineKey(paths), value) => paths.reverse.toList match
        case head :: tail => unfoldPaths(tail, (head, value))
        case _ => tuple
      case a@_ => a
    })

  
  private def objPatternMatch(value: Map[String, Variable], input: JsValue,
                              results: collection.mutable.Map[String, Any]): Unit =
    
    assert(input.isInstanceOf[JsObject])
    val inputObj = input.asInstanceOf[JsObject]
    
    val (_declared, others) = value.partition(_._2.pattern != AnyVals())
    
    others.values.headOption match
      case Some(_) => assert(_declared.keys.toSet -- inputObj.fields.keys.toSet == Set.empty)
      case _ => assert(_declared.keys.toSet == inputObj.fields.keys.toSet)
    
    value.foreach {
      case (_, x@Variable(_, JsPattern.AnyVals())) =>
        val declaredKeys = _declared.keys.toSet
        patternMatch(x, JsObject(inputObj.fields.filterNot { case (k, _) => declaredKeys.contains(k) }), results)

      case (key, _value) =>
        assert(inputObj.fields contains key)
        patternMatch(_value, inputObj.fields(key), results)
    }
  
  private def arrPatternMatch(value: List[Variable], input: JsValue,
                              results: collection.mutable.Map[String, Any]): Unit =
    assert(input.isInstanceOf[JsArray])
    val inputArr = input.asInstanceOf[JsArray]
    
    if (value.exists(_.pattern == JsPattern.AnyVals())) then
      assert(input.asInstanceOf[JsArray].elements.size >= value.size - 1)
    else
      assert(input.asInstanceOf[JsArray].elements.size == value.size)

    value.zipWithIndex.foreach {
      case (x@Variable(_, JsPattern.AnyVals()), idx) =>
        patternMatch(x, JsArray(inputArr.elements.takeRight(inputArr.elements.size - idx)), results)

      case (x: Variable, y: Int) =>
        assert(inputArr.elements.size > y)
        patternMatch(x, inputArr.elements(y), results)
    }

  private def setResult[T](results: collection.mutable.Map[String, Any], name: String, value: T): Unit = {
    name match
      case null =>
      case Placeholder(index) => results(index) = value
      case s: String => results(s) = value
  }
  
  private def patternMatch(pattern: Variable, input: JsValue,
                           results: collection.mutable.Map[String, Any]): Unit = pattern match
    
    case Variable(name, JsPattern.ObjPattern(value)) =>
      assert(input.isInstanceOf[JsObject])
      setResult(results, name, input.asInstanceOf[JsObject])
      objPatternMatch(exactInlineKeys(value), input, results)
    
    case Variable(name, JsPattern.ArrPattern(value)) =>
      assert(input.isInstanceOf[JsArray])
      setResult(results, name, input.asInstanceOf[JsArray])
      arrPatternMatch(value, input, results)
    
    case Variable(name, JsPattern.AnyVal(GroundType.STRING)) =>
      assert(input.isInstanceOf[JsString])
      setResult(results, name, input.asInstanceOf[JsString].value)
    
    case Variable(name, JsPattern.AnyVal(GroundType.INTEGER)) =>
      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value.isValidInt)
      setResult(results, name, input.asInstanceOf[JsNumber].value.toInt)
      
    case Variable(name, JsPattern.AnyVal(GroundType.NUMBER)) =>
      assert(input.isInstanceOf[JsNumber])
      setResult(results, name, input.asInstanceOf[JsNumber].value)
    
    case Variable(name, JsPattern.AnyVal(GroundType.BOOLEAN)) =>
      assert(input.isInstanceOf[JsBoolean])
      setResult(results, name, input.asInstanceOf[JsBoolean].value)
    
    case Variable(name, JsPattern.AnyVal(GroundType.ANY) | JsPattern.AnyVals()) =>
      setResult(results, name, input)
    
    case Variable(name, JsPattern.StringPattern(value)) =>
      assert(input.isInstanceOf[JsString] && input.asInstanceOf[JsString].value == value)
      setResult(results, name, value)
    
    case Variable(name, JsPattern.NumberPattern(value)) =>
      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value == value)
      setResult(results, name, value)
    
    case Variable(name, JsPattern.BoolPattern(value)) =>
      assert(input.isInstanceOf[JsBoolean] && input.asInstanceOf[JsBoolean].value == value)
      setResult(results, name, value)
    
    case Variable(name, JsPattern.TaggedString(tag, content)) =>
      // todo
      assert(true)
    case Variable(_, JsPattern.AnyVals()) => // TODO

    case Variable(_, JsPattern.NullPattern()) => // TODO
    case Variable(_, JsPattern.AnyVal(GroundType.ARRAY)) => // TODO
    case Variable(_, JsPattern.AnyVal(GroundType.OBJECT)) => // TODO
