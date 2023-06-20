package wjson

import wjson.JsValue.JsNull

import scala.collection.SortedMap

/**
 * Created by wangzx on 15/7/6.
 */

class JsonInterpolation(sc: StringContext) {

  object Placeholder {
    def apply(index: Int): JsValue = JsString("\u0000_placeholder_" + index + "_\u0000")
    def unapply(str: JsValue): Option[Int] = {
      str match
        case JsString(str) if str.startsWith("\u0000_placeholder_") && str.endsWith("_\u0000") =>
          Some(str.substring(14, str.length - 2).nn.toInt)
        case _ => None
    }
  }

  protected def parse(input: ParserInput): JsValue =
    new JsonParser(input).parseJsValue()


  def apply(args: JsValue*): JsValue =
    parse( ParserInput(sc, args) )
//    new JsonParser(ParserInput(sc, args) ).parseJsValue()

  def unapplySeq(input: JsValue): Option[Seq[JsValue]] = {

    val placeHolders: Seq[JsValue] = Seq.range(0, sc.parts.length-1)
      .map(x => Placeholder(x) )

    val pi = ParserInput(sc, placeHolders)
//    val pattern = new JsonParser(pi ).parseJsValue()
    val pattern = parse( pi )

    val results = collection.mutable.ArrayBuffer[JsValue]()
    Seq.range(0, sc.parts.length-1).foreach { x => results += JsNull }

    try {
      patternMatch(pattern, input, placeHolders, results)
      Some(results.toSeq)
    }
    catch {
      case ex: Throwable => None
    }

  }

  // TODO report friendly
  private def patternMatch(pattern: JsValue, input: JsValue, placeHolders: Seq[JsValue],
                           results: collection.mutable.ArrayBuffer[JsValue]): Unit = {

    pattern match {
      case p_o: JsObject =>
        assert( input.isInstanceOf[JsObject] )
        val inputObj = input.asInstanceOf[JsObject]

        p_o.fields.foreach {
          case (key, Placeholder(index))  =>
            assert( inputObj.contains(key) )
            results(index) = inputObj.field(key)

          case (key, value) =>
            assert(inputObj.contains(key))
            patternMatch(value, inputObj.field(key), placeHolders, results)
        }

      case p_a: JsArray =>
        assert(input.isInstanceOf[JsArray] &&
               input.asInstanceOf[JsArray].elements.size >= p_a.elements.size)
        val inputArr = input.asInstanceOf[JsArray]

        p_a.elements.zipWithIndex.foreach {
          case (Placeholder(index), y: Int) =>
            results(index) = inputArr.elements(y)
          case (x: JsValue,y: Int)=>
            patternMatch(x, inputArr.elements.apply(y), placeHolders, results)
        }

      case p: JsString =>
        assert(p == input)
      case p: JsBoolean =>
        assert(p == input)
      case p: JsNumber =>
        assert(p == input)
      case p@ JsNull =>
        assert(p == input)
    }
  }


}