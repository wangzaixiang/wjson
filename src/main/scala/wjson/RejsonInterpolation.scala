package wjson

import com.sun.xml.internal.bind.v2.TODO
import wjson.*
import wjson.JsPattern.Variable
import wjson.JsPattern.*

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/**
 * rejson is a pattern language for JSON
 */
class RejsonInterpolation(sc: StringContext):
  
  object Placeholder:
    def apply(index: Int): JsString = JsString("_placeholder_" + index + "_")
    def unapply(str: String): Option[Int] =
      str match
        case str if str.startsWith("_placeholder_") && str.endsWith("_") =>
          Some(str.substring(13, str.length - 1).toInt)
        case _ => None

  object InlineKey:
    def unapply(str: String): Option[Seq[String]] =
      str match
        case str if str.contains("/") =>
          Some(str.split("/").toSeq.filter(_.nonEmpty))
        case _ => None

  
  def unapplyAsMap(input: JsValue): Option[Map[String, Any]] = ???
  
  def unapplySeq(input: JsValue): Option[Seq[Any]] =
    
    val str = if sc.parts.size > 1 then
      sc.parts.head + sc.parts.tail.zipWithIndex.map { case (p, idx) => Placeholder(idx).value + p }.mkString
    else sc.parts.head
    
    val pattern = JsPatternParser.parseRejson(str)

    val results = collection.mutable.ArrayBuffer[Any]()
    Seq.range(0, sc.parts.length - 1).foreach { x => results += null }
    
    try
      patternMatch(pattern, input, results)
      Some(results.toSeq)
    catch
      case ex: Throwable =>
        None

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
          val value = leftVariables.zipAll(rightVariables, null, null).zipWithIndex.map {
            case ((null, right), _) => right
            case ((left, null), _) => left
            case ((left@Variable(_, ObjPattern(_)), right@Variable(_, ObjPattern(_))), idx) =>
              deepMerge(idx.toString, Some(left), Some(right))
            case ((left@Variable(_, ArrPattern(_)), right@Variable(_, ArrPattern(_))), idx) =>
              deepMerge(idx.toString, Some(left), Some(right))
            case ((Variable(leftName, leftPtn), Variable(rightName, rightPtn)), _) =>
              val varName = getVariableName(Set(leftName, rightName), key)
              assert(leftPtn == rightPtn, "different patterns in array")
              Variable(varName, leftPtn)
          }

          val anyVals = (leftAnyVals.map(_.name) ::: rightAnyVals.map(_.name)).distinct.map(Variable(_, AnyVals()))
          assert(anyVals.size <= 1, s"more than one variable name for: ${key}")
          
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
        case head :: tail => unfoldPaths(tail, (head -> Variable(null, ObjPattern(Map(thisMap._1 -> thisMap._2)))))
        case _ => thisMap
    
    merge(unmerge.toList.map {
      case tuple@(InlineKey(paths), value) => paths.reverse.toList match
        case head :: tail => unfoldPaths(tail, (head, value))
        case _ => tuple
      case a@_ => a
    })

  
  private def objPatternMatch(value: Map[String, Variable], input: JsValue,
                              results: collection.mutable.ArrayBuffer[Any]): Unit =
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
      case (key, Variable(Placeholder(index), ptn: (ArrPattern | ObjPattern))) =>
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
  
  private def arrPatternMatch(value: List[Variable], input: JsValue,
                              results: collection.mutable.ArrayBuffer[Any]): Unit =
    assert(input.isInstanceOf[JsArray])
    val inputArr = input.asInstanceOf[JsArray]
    
    if (value.exists(_.pattern == JsPattern.AnyVals())) then
      assert(input.asInstanceOf[JsArray].elements.size >= value.size - 1)
    else
      assert(input.asInstanceOf[JsArray].elements.size == value.size)

    value.zipWithIndex.foreach {
      case (Variable(Placeholder(index), JsPattern.AnyVals()), idx) =>
        results(index) = inputArr.elements.takeRight(inputArr.elements.size - idx)
      case (Variable(_, JsPattern.AnyVals()), _) =>

      case (x@Variable(Placeholder(index), ptn: (ArrPattern | ObjPattern)), y: Int) =>
        results(index) = parseType(inputArr.elements(y), ptn)
        patternMatch(x, inputArr.elements(y), results)

      case (Variable(Placeholder(index), ptn), y: Int) =>
        results(index) = parseType(inputArr.elements(y), ptn)

      case (x: Variable, y: Int) =>
        patternMatch(x, inputArr.elements(y), results)
    }

  
  private def patternMatch(pattern: Variable, input: JsValue, results: collection.mutable.ArrayBuffer[Any]): Unit = pattern match
    
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
    
    case Variable(_, JsPattern.TaggedString(tag, content)) => // todo
      assert(true)
    case Variable(_, JsPattern.AnyVals()) => // TODO

    case Variable(_, JsPattern.NullPattern()) => // TODO
    case Variable(_, JsPattern.AnyVal(GroundType.ARRAY)) => // TODO
    case Variable(_, JsPattern.AnyVal(GroundType.OBJECT)) => // TODO
