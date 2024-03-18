package wjson

//import org.mvel2.MVEL
import wjson.*
import wjson.JsPattern.*
import wjson.JsValue.{JsArray, JsNull, JsObject}
import wjson.RejsonMatcher.Placeholder

import scala.annotation.tailrec
import scala.collection.mutable.Map as MutableMap
import scala.jdk.CollectionConverters.*

class RejsonInterpolation(sc: StringContext):

  def unapplySeq(input: JsValue): Option[Seq[Any]] =

    val str = if sc.parts.size > 1 then
      sc.parts.head + sc.parts.tail.zipWithIndex.map { case (p, idx) => Placeholder(idx) + p }.mkString
    else sc.parts.head

    val pattern = JsPatternParser.parseRejson(str)

    new RejsonMatcher(pattern).unapplyAsMap(input)
      .map( results => Seq.range(0, sc.parts.length-1).map( i => results(Placeholder(i)) ) )

object RejsonMatcher:

  object Placeholder:
    def apply(index: Int): String = "_placeholder_" + index + "_"
    def unapply(str: String): Option[Int] = str match
      case str if str.startsWith("_placeholder_") && str.endsWith("_") =>
        Some(str.substring(13, str.length - 1).toInt)
      case _ => None

/**
 * rejson is a pattern language for JSON
 */
case class RejsonMatcher(pattern: JsPattern.Variable):

  def this(program: String) = this( JsPatternParser.parseRejson(program) )

  def unapplyAsMap(input: JsValue, throwable: Boolean = false): Option[Map[String, Any]] =
    val results = MutableMap[String, Any]()

    val m1 = patternMatch(pattern, input, results, throwable)
    if(m1) Some(results.toMap) else None

  // does input match arrPattern?
  private def arrPatternMatch(arrPattern: ArrPattern, input: JsValue, results: MutableMap[String, Any]): Boolean =
    input match
      case jsa: JsArray =>
          val has_anys = arrPattern.value.exists(_.pattern == AnyVals())
          if has_anys then
            val head = arrPattern.value.takeWhile(_.pattern != AnyVals())
            val anys :: tail = arrPattern.value.dropWhile(_.pattern != AnyVals()) : @unchecked

            jsa.elements.size >= (head.size + tail.size) && {
              val a_head = jsa.elements.slice(0, head.size)
              val a_tail = jsa.elements.slice(jsa.elements.length - tail.size, jsa.elements.length)
              val a_anys = jsa.elements.slice(head.size, jsa.elements.size - tail.size)

              val matches =
                head.zip(a_head).forall { case (variable, input) =>
                  patternMatch(variable, input, results)
                } &&
                tail.zip(a_tail).forall { case (variable, input) =>
                  patternMatch(variable, input, results)
                }

              if (matches && anys.name != null)
                results(anys.name) = JsArray(a_anys)
              matches
            }
          else // has_anys == false
            jsa.elements.size >= arrPattern.value.size && arrPattern.value.zip(jsa.elements).forall { case(variable, input) =>
              patternMatch(variable, input, results)
            }
      case _ => false

  @tailrec
  private def getElementByPath(jsv: JsValue, path: Path): JsValue | Seq[JsValue] =
    path.value match
      case PathElement.Index(index) :: tail =>
        jsv match
          case array: JsArray if array.elements.size > index =>
            val finalIndex = if (index < 0) array.elements.size + index else index
            if (finalIndex >= 0) then getElementByPath( array.elements(finalIndex), Path(tail) )
            else JsNull
          case _ =>  JsNull
      case PathElement.Simple(simple) :: tail =>
        jsv match
          case jso: JsObject =>
            getElementByPath(jso.field(simple), Path(tail))
          case _ => JsNull
      case PathElement.ArrayFilter(pattern) :: tail =>
        jsv match
          case array: JsArray =>
            val filtered = array.elements.filter( elem => patternMatch(Variable(null, pattern), elem, MutableMap()) )
            getElementByPath(filtered, Path(tail))
          case _ => JsNull
      case Nil => jsv

  private def getElementByPath(arr: Seq[JsValue], path: Path): JsValue | Seq[JsValue] =
    path.value match
      case PathElement.Index(index) :: tail =>  // arr -> elem
        if(arr.size > index) then getElementByPath(arr(index), Path(tail))
        else JsNull
      case head :: tail =>
        val part0 = arr.flatMap { elem =>
          getElementByPath(elem, Path(List(head))) match
            case x: JsValue => Seq(x)
            case x: Seq[JsValue]@unchecked => x
        }
        getElementByPath(part0, Path(tail))
      case Nil => arr

  // does input match the objPattern?
  private def objPatternMatch(objPattern: ObjPattern, input: JsValue, results: MutableMap[String, Any]): Boolean =
    input match
      case jso: JsObject =>
          val has_anys = objPattern.value.exists(_._2.pattern == AnyVals())
          if has_anys then
            val anys: Variable = objPattern.value.filter(_._2.pattern == AnyVals()).apply(0)._2
            val not_anys = objPattern.value.filter(_._2.pattern != AnyVals())
            val not_anys_keys = not_anys.map { case (path, pattern) =>  // declared fields
              path.value(0).asInstanceOf[PathElement.Simple].value
            }.toSet //(_._1.value(0).value)
            val matches = not_anys.forall { case (key, variable: Variable) =>
              getElementByPath(jso, key) match
                case x: JsValue => patternMatch(variable, x, results)
                case x: Seq[JsValue]@unchecked => patternMatch(variable, JsArray(x), results)
            }
            if matches && anys.name != null then // bound anys
               results(anys.name) = JsObject(jso.fields.filterNot( x => not_anys_keys.contains(x._1) ))
            matches
          else
            objPattern.value.forall { case (key, variable) =>
              val elem = getElementByPath(jso, key)
              elem match
                case x: JsValue => patternMatch(variable, x, results)
                case x: Seq[JsValue]@unchecked => patternMatch(variable, JsArray(x), results)
            }
      case _ => false

  // pojo values used in MVEL expression
  private def asPojo(value: JsValue): AnyRef = value match
    case JsNull => null
    case JsBoolean(v) => new java.lang.Boolean(v)
    case JsNumber(v: Double) => new java.lang.Double(v)
    case JsNumber(v: Long) => new java.lang.Long(v)
    case JsString(v) => v
    case JsArray(v) => v.map(asPojo).toArray
    case JsObject(v) => v.map(x => (x._1, asPojo((x._2)))).toMap.asJava

  // TODO enable extension tags in a better API
  private def tagStringMatch(tag: String, content: String, value: JsValue, results: MutableMap[String, Any]): Boolean =
    val context = Map("it"->asPojo(value), "js" -> value)
    if tag == "eval" then
      Eval.eval(content, context) == true
    else if tag == "r" && value.isInstanceOf[JsString] then
      java.util.regex.Pattern.compile(content).matcher(value.asInstanceOf[JsString].value).matches()
    else
      false

  extension (bool: Boolean)
    def ifTrue( result: =>Any ): Option[Any] = if(bool) Some(result) else None

  private def patternMatch(variable: Variable, input: JsValue,
                           results: MutableMap[String, Any], throwable: Boolean = false): Boolean =
    import JsPattern.*

    val value =
    variable.pattern match
      case NullPattern() => (input == JsNull) ifTrue JsNull
      case BoolPattern(value: Boolean) =>  (input == JsBoolean(value)) ifTrue  input.asInstanceOf[JsBoolean].value
      case NumberPattern(value) => (input == JsNumber(value)) ifTrue  input.asInstanceOf[JsNumber].value
      case StringPattern(value: String) => (input == JsString(value)) ifTrue input.asInstanceOf[JsString].value
      case a@ArrPattern(value: Seq[JsPattern.Variable]) =>  arrPatternMatch(a, input, results) ifTrue  input
      case o@ObjPattern(value: Seq[(JsPattern.Path, JsPattern.Variable)]) =>  objPatternMatch(o, input, results) ifTrue input
      case AnyVal(GroundType.NUMBER) => input.isInstanceOf[JsNumber] ifTrue input.asInstanceOf[JsNumber].value
      case AnyVal(GroundType.INTEGER) =>
        input match
          case JsNumber(x:Long) => Some(x)
          case _ => None
      case AnyVal(GroundType.STRING) => input.isInstanceOf[JsString] ifTrue input.asInstanceOf[JsString].value
      case AnyVal(GroundType.BOOLEAN) => input.isInstanceOf[JsBoolean] ifTrue input.asInstanceOf[JsBoolean].value
      case AnyVal(GroundType.OBJECT) => input.isInstanceOf[JsObject] ifTrue input.asInstanceOf[JsObject].fields
      case AnyVal(GroundType.ARRAY) => input.isInstanceOf[JsArray] ifTrue input.asInstanceOf[JsArray].elements
      case AnyVal(GroundType.ANY) => true ifTrue input
      case AnyVals() => throw new RuntimeException("_* not supported to using here")
      case TaggedString(tag:String, content:String) => tagStringMatch(tag, content, input, results) ifTrue input // TODO

    value match
      case Some(v) if variable.name != null =>
        results(variable.name) = if(v==JsNull) null else v
        true
      case Some(v) => true
      case None  => {
        if (throwable) {
          val msg = s"Expected pattern [${variable.pattern}] but got [${input}]"
          throw new Exception(msg)
        }
        false
      }


