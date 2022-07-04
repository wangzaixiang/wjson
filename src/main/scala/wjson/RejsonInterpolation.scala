package wjson

import wjson.*
import wjson.JsPattern.Variable
import wjson.JsPattern.*
import wjson.JsValue
import wjson.JsValue.{JsArray, JsBoolean, JsEmptyString, JsNull, JsNumber, JsObject}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/**
 * rejson is a pattern language for JSON
 */
class RejsonInterpolation(sc: StringContext):
  
  object Placeholder:
    def apply(index: Int): String = "_placeholder_" + index + "_"
    def unapply(str: String): Option[Int] =
      str match
        case str if str.startsWith("_placeholder_") && str.endsWith("_") =>
          Some(str.substring(13, str.length - 1).toInt)
        case _ => None

  object InlineKey:
    def unapply(str: String): Option[List[String]] =
      str match
        case str if str.contains("/") =>
          Some(str.split("/").toList.filter(_.nonEmpty))
        case _ => None

  def preprocess(variable: Variable): Variable = variable.copy( pattern = preprocess(variable.pattern) )
  def preprocess(pattern: JsPattern): JsPattern = pattern match
    case ArrPattern(value) => ArrPattern(value.map(preprocess))
    case ObjPattern(value: Map[String, Variable]) =>
      val a = value.toList
      val aa = a
        .map { case(k,v) =>
          k match
            case InlineKey(head :: tail) =>
              (head, expand(tail, preprocess(v)))
            case _ =>
              (k, preprocess(v))
         }
      val b = aa.groupBy(_._1)
      val c = b.map { case (k, vs) =>
        if(vs.size == 1) (k, vs(0)._2)
        else (k, merge(vs.map(_._2)))
      }

      ObjPattern(c)
    case _ => pattern

  // 1 AnyVal, other ObjPattern
  def merge(vs: List[Variable]): Variable =
    vs.reduce { (var1, var2) =>
      val names = Set(var1.name, var2.name).filter(_ != null)
      var name: String =
        if( names.size == 1 ) { if(var1.name != null) var1.name else var2.name }
        else if(names.size == 0) null
        else ???

      if(var1.pattern == AnyVal(GroundType.ANY)) var2.copy(name = name)
      else if(var2.pattern == AnyVal(GroundType.ANY)) var1.copy(name = name)
      else if(var1.pattern.isInstanceOf[ObjPattern]){
        assert( var1.pattern.isInstanceOf[ObjPattern])
        assert( var2.pattern.isInstanceOf[ObjPattern])
//        Variable(name, ObjPattern( var1.pattern.asInstanceOf[ObjPattern].value ))
        val p1 = var1.pattern.asInstanceOf[ObjPattern]
        val p2 = var2.pattern.asInstanceOf[ObjPattern]

        Variable(name, ObjPattern( (p1.value.keys ++ p2.value.keys).map { k =>
           val v1 = p1.value.get(k)
           val v2 = p2.value.get(k)
           val v = (v1, v2) match
             case (Some(a),Some(b)) => merge(List(a,b))
             case (Some(a), None) => a
             case (None, Some(b)) => b
             case _ => ???
           (k, v)
        }.toMap ) )
      }
      else if(var1.pattern.isInstanceOf[ArrPattern]){  //
        // [ a, b, c, anys ]
        // [ a, b, c, d, e ]
        val p1 = var1.pattern.asInstanceOf[ArrPattern]
        val p2 = var2.pattern.asInstanceOf[ArrPattern]

        assert( p1.value.forall(_.pattern != AnyVals()) )
        assert( p2.value.forall(_.pattern != AnyVals()) )

        val size = Math.max( p1.value.length, p2.value.length)
        val arr = List.range(0, size).map { i =>
          val e1 = if(i < p1.value.length) Some(p1.value(i)) else None
          val e2 = if(i < p2.value.length) Some(p2.value(i)) else None
          (e1, e2) match
            case (Some(a), Some(b)) => merge(List(a,b))
            case (Some(a), None) => a
            case (None, Some(b)) => b
            case _ => ???
        }
        Variable(name, ArrPattern(arr))
      }
      else ???
    }

  //@tailrec
  def expand(path: List[String], variable:Variable): Variable =
    path match
      case head :: Nil => Variable(null, ObjPattern( Map(head->variable)))
      case head :: tail =>
        Variable(null, ObjPattern(Map(head -> expand(tail, variable))))
      case Nil => ???

  def unapplyAsMap(input: JsValue): Option[Map[String, Any]] =
    
    val str = if sc.parts.size > 1 then
      sc.parts.head + sc.parts.tail.zipWithIndex.map { case (p, idx) => Placeholder(idx) + p }.mkString
    else sc.parts.head

    val pattern = JsPatternParser.parseRejson(str)
    val pattern2 = preprocess(pattern)
    
    val results = collection.mutable.Map[String, Any]()
    
    val m1 = patternMatch(pattern2, input, results)
    if(m1) Some(results.toMap) else None


  def unapplySeq(input: JsValue): Option[Seq[Any]] =
    val variables = unapplyAsMap(input)
    variables.map( map => Seq.range(0, sc.parts.length-1).map( i => map(Placeholder(i)) ) )

  def exactInlineKeys(unmerge: Map[String, Variable]): Map[String, Variable] =
    
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

//
//  private def objPatternMatch(value: Map[String, Variable], input: JsValue,
//                              results: collection.mutable.Map[String, Any]): Unit =
//
//    assert(input.isInstanceOf[JsObject])
//    val inputObj = input.asInstanceOf[JsObject]
//
//    val (_declared, others) = value.partition(_._2.pattern != AnyVals())
//
//    others.values.headOption match
//      case Some(_) => assert(_declared.keys.toSet -- inputObj.fields.keys.toSet == Set.empty)
//      case _ => assert(_declared.keys.toSet == inputObj.fields.keys.toSet)
//
//    value.foreach {
//      case (_, x@Variable(_, JsPattern.AnyVals())) =>
//        val declaredKeys = _declared.keys.toSet
//        patternMatch(x, JsObject(inputObj.fields.filterNot { case (k, _) => declaredKeys.contains(k) }), results)
//
//      case (key, _value) =>
//        assert(inputObj.fields contains key)
//        patternMatch(_value, inputObj.fields(key), results)
//    }
//
//  private def arrPatternMatch(value: List[Variable], input: JsValue,
//                              results: collection.mutable.Map[String, Any]): Unit =
//    assert(input.isInstanceOf[JsArray])
//    val inputArr = input.asInstanceOf[JsArray]
//
//    if (value.exists(_.pattern == JsPattern.AnyVals())) then
//      assert(input.asInstanceOf[JsArray].elements.size >= value.size - 1)
//    else
//      assert(input.asInstanceOf[JsArray].elements.size == value.size)
//
//    value.zipWithIndex.foreach {
//      case (x@Variable(_, JsPattern.AnyVals()), idx) =>
//        patternMatch(x, JsArray(inputArr.elements.takeRight(inputArr.elements.size - idx)), results)
//
//      case (x: Variable, y: Int) =>
//        assert(inputArr.elements.size > y)
//        patternMatch(x, inputArr.elements(y), results)
//    }

//  private def setResult[T](results: collection.mutable.Map[String, Any], name: String, value: T): Unit = {
//    name match
//      case null =>
//      case Placeholder(index) => results(index) = value
//      case s: String => results(s) = value
//  }

  def slice[T](array: Seq[T], from: Int, size: Int): Seq[T] =
    array.slice(from, from+size)

  def arrPatternMatch2(arrPattern: ArrPattern, input: JsValue, results: collection.mutable.Map[String, Any]): Boolean =
    val has_anys: Boolean = arrPattern.value.exists(_.pattern == AnyVals())
    assert(input.isInstanceOf[JsArray])
    val jsarray = input.asInstanceOf[JsArray]

    if has_anys then
      val head = arrPattern.value.takeWhile(_.pattern != AnyVals())
      val it :: tail = arrPattern.value.dropWhile(_.pattern != AnyVals())

      assert(jsarray.elements.size >= (head.size + tail.size))
      val a_head = slice(jsarray.elements, 0, head.size)
      val a_tail = slice(jsarray.elements, jsarray.elements.length - tail.size, tail.size)
      val a_anys = slice(jsarray.elements, head.size, jsarray.elements.size - head.size - tail.size)

      val m1 = head.zip(a_head).forall { case(variable, input) =>
        patternMatch(variable, input, results)
      }

      val m2 = tail.zip(a_tail).forall { case(variable, input) =>
        patternMatch(variable, input, results)
      }

      if(it.name != null)
        results(it.name) = JsArray(a_anys)
      m1 && m2

    else
      assert( jsarray.elements.size >= arrPattern.value.size )
      arrPattern.value.zip(jsarray.elements).forall { case(variable, input) =>
        patternMatch(variable, input, results)
      }

  def objPatternMatch2(objPattern: ObjPattern, input: JsValue, results: collection.mutable.Map[String, Any]): Boolean =
    assert(input.isInstanceOf[JsObject])
    val jso = input.asInstanceOf[JsObject]
    val has_anys = objPattern.value.exists(_._2.pattern == AnyVals())
    if has_anys then
      val others:  Map[String, JsPattern.Variable] = objPattern.value.filter(_._2.pattern != AnyVals())
      val the_anys: Variable = objPattern.value.filter(_._2.pattern == AnyVals()).values.toSeq.apply(0)
      val m1 = others.forall { case (key, variable: Variable) =>
        patternMatch(variable, jso.fields.get(key).getOrElse(JsNull), results)
      }
      if(m1 && the_anys.name != null){
         results(the_anys.name) = JsObject(jso.fields.filterNot( x => others.contains(x._1) ))
      }
      m1
    else
      objPattern.value.forall { case (key, variable)=>
        patternMatch(variable, jso.fields.get(key).getOrElse(JsNull), results)
      }

  extension (bool: Boolean)
    def *( result: =>Any ): Option[Any] = if(bool) Some(result) else None

  private def patternMatch(variable: Variable, input: JsValue,
                           results: collection.mutable.Map[String, Any]): Boolean =
    import JsPattern.*

    val value =
    variable.pattern match
      case NullPattern() => (input == JsNull) * JsNull
      case BoolPattern(value: Boolean) =>  (input == JsBoolean(value)) * input.asInstanceOf[JsBoolean].value
      case NumberPattern(value: Double) => (input == JsNumber(value)) * input.asInstanceOf[JsNumber].value
      case StringPattern(value: String) => (input == JsString(value)) * input.asInstanceOf[JsString].value
      case a@ArrPattern(value: List[JsPattern.Variable]) =>  arrPatternMatch2(a, input, results) * input
      case o@ObjPattern(value: Map[String, JsPattern.Variable]) =>  objPatternMatch2(o, input, results) * input
      case AnyVal(GroundType.NUMBER) => input.isInstanceOf[JsNumber] * input.asInstanceOf[JsNumber].value
      case AnyVal(GroundType.INTEGER) => input.isInstanceOf[JsNumber] * input.asInstanceOf[JsNumber].value.toInt
      case AnyVal(GroundType.STRING) => input.isInstanceOf[JsString] * input.asInstanceOf[JsString].value
      case AnyVal(GroundType.BOOLEAN) => input.isInstanceOf[JsBoolean] * input.asInstanceOf[JsBoolean].value
      case AnyVal(GroundType.OBJECT) => input.isInstanceOf[JsObject] * input.asInstanceOf[JsObject].fields
      case AnyVal(GroundType.ARRAY) => input.isInstanceOf[JsArray] * input.asInstanceOf[JsArray].elements
      case AnyVal(GroundType.ANY) => true * input
      case AnyVals() => ???    // _*: match any number of JsVals
      case TaggedString(tag:String, content:String) => ???

    value match
      case Some(v) if variable.name != null =>
        results(variable.name) = if(v==JsNull) null else v
        true
      case Some(v) => true
      case None  => false


//    pattern match
//
//    case Variable(name, JsPattern.ObjPattern(value)) =>
//      assert(input.isInstanceOf[JsObject])
//      setResult(results, name, input.asInstanceOf[JsObject])
//      objPatternMatch(exactInlineKeys(value), input, results)
//
//    case Variable(name, JsPattern.ArrPattern(value)) =>
//      assert(input.isInstanceOf[JsArray])
//      setResult(results, name, input.asInstanceOf[JsArray])
//      arrPatternMatch(value, input, results)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.STRING)) =>
//      assert(input.isInstanceOf[JsString])
//      setResult(results, name, input.asInstanceOf[JsString].value)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.INTEGER)) =>
//      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value.isValidInt)
//      setResult(results, name, input.asInstanceOf[JsNumber].value.toInt)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.NUMBER)) =>
//      assert(input.isInstanceOf[JsNumber])
//      setResult(results, name, input.asInstanceOf[JsNumber].value)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.BOOLEAN)) =>
//      assert(input.isInstanceOf[JsBoolean])
//      setResult(results, name, input.asInstanceOf[JsBoolean].value)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.ARRAY)) =>
//      assert(input.isInstanceOf[JsArray])
//      setResult(results, name, input.asInstanceOf[JsArray].elements)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.OBJECT)) =>
//      assert(input.isInstanceOf[JsObject])
//      setResult(results, name, input.asInstanceOf[JsObject].fields)
//
//    case Variable(name, JsPattern.AnyVal(GroundType.ANY) | JsPattern.AnyVals()) =>
//      setResult(results, name, input)
//
//    case Variable(name, JsPattern.StringPattern(value)) =>
//      assert(input.isInstanceOf[JsString] && input.asInstanceOf[JsString].value == value)
//      setResult(results, name, value)
//
//    case Variable(name, JsPattern.NumberPattern(value)) =>
//      assert(input.isInstanceOf[JsNumber] && input.asInstanceOf[JsNumber].value == value)
//      setResult(results, name, value)
//
//    case Variable(name, JsPattern.BoolPattern(value)) =>
//      assert(input.isInstanceOf[JsBoolean] && input.asInstanceOf[JsBoolean].value == value)
//      setResult(results, name, value)
//
//    case Variable(name, JsPattern.TaggedString(tag, content)) =>
//      // todo
//      assert(true)
//
//    case Variable(name, JsPattern.NullPattern()) =>
//      assert(input == JsNull)
//      setResult(results, name, null)
      
