package wjson

import wjson.JsValue.{JsArray, JsNumber}

import scala.annotation.targetName
import scala.collection.mutable

/**
 * Json Model ADT
 */
enum JsValue:
    case JsNull
    case JsBoolean(value: Boolean)
    case JsNumber(value: Double|Long)
    case JsString(value: String)
    case JsArray(elements: Seq[JsValue])
    case JsObject(fields: Seq[(String, JsValue)])  // changed, preserve the order of fields

    // compare JsValue ignore the order of fields
    override def equals(obj: Any): Boolean = obj match
      case x: JsBoolean => this.isInstanceOf[JsBoolean] && x.value == this.asInstanceOf[JsBoolean].value
      case x: JsNumber => this.isInstanceOf[JsNumber] && x.value == this.asInstanceOf[JsNumber].value
      case x: JsString => this.isInstanceOf[JsString] && x.value == this.asInstanceOf[JsString].value
      case x: JsArray => this.isInstanceOf[JsArray] && x.elements == this.asInstanceOf[JsArray].elements
      case x: JsObject => this.isInstanceOf[JsObject] && x.fields.toMap == this.asInstanceOf[JsObject].fields.toMap
      case x: JsValue => this.eq(JsNull)
      case _ => false


object JsValue:
  val JsTrue: JsBoolean = JsBoolean(true)
  val JsFalse: JsBoolean = JsBoolean(false)
  val JsZero: JsNumber = JsNumber(0L)
  val JsEmptyString: JsString = JsString("")
  val JsEmptyObject: JsObject = JsObject(Seq.empty)
  val JsEmptyArray: JsArray = JsArray(Seq.empty)

  def JsNumber(value: Int): JsNumber = JsNumber(value.toLong)
  def parseJson(str: String): JsValue = JsonParser.parse(ParserInput(str))
  def parseJson5(str: String): JsValue = new Json5Parser(ParserInput(str)).parseJsValue()
  def obj(fields: (String, JsValue)*): JsObject = JsObject(fields)
  def arr(elements: JsValue*): JsArray = JsArray(elements)

  extension (value: JsObject)
    def contains(name: String): Boolean = value.fields.exists(_._1 == name)
    def field(name: String): JsValue = value.fields.find(_._1 == name).map(_._2).get
    def fieldOpt(name: String): Option[JsValue] = value.fields.find(_._1 == name).map(_._2)

    def merge(kvs: (String, JsValue)*): JsObject =
      val keys1 = value.fields.map(_._1).toSet
      val keys2 = kvs.map(_._1).toSet
      val duplicateKeys = keys1.intersect(keys2)
      if duplicateKeys.nonEmpty then
        JsObject(value.fields.filterNot(x => duplicateKeys.contains(x._1)) ++ kvs)
      else JsObject(value.fields ++ kvs)

    @targetName("appendObject")
    def ++(other: JsObject): JsObject = merge(other.fields:_*)

    @targetName("appendSeq")
    def ++(other: Seq[(String, JsValue)]): JsObject = merge(other:_*)

    @targetName("append")
    def +(kv: (String, JsValue)): JsObject =
      if value.fields.exists(_._1 == kv._1) then
        JsObject(value.fields.filterNot(_._1 == kv._1) :+ kv)
      else JsObject(value.fields :+ kv)

  extension (value: JsArray)

    @targetName("append")
    def :+(elem: JsValue): JsArray = JsArray(value.elements :+ elem)

    @targetName("appendSeq")
    def ++(other: JsArray): JsArray = JsArray(value.elements ++ other.elements)

    @targetName("prepend")
    def +:(elem: JsValue): JsArray = JsArray(elem +: value.elements)

  extension (value: JsValue)
    def show: String = show(0)
    def showPretty: String = show(indent = 2)

    def asStr: JsString = value match
      case x: JsString => x
      case _ => throw new Exception(s"expect JsString, but got $value")

    def asNum: JsNumber = value match
      case x: JsNumber => x
      case _ => throw new Exception(s"expect JsNumber, but got $value")

    def asArr: JsArray = value match
      case x: JsArray => x
      case _ => throw new Exception(s"expect JsArray, but got $value")

    def asObj : JsObject = value match
      case x: JsObject => x
      case _ => throw new Exception(s"expect JsObject, but got $value")

    def asBool : JsBoolean = value match
      case x: JsBoolean => x
      case _ => throw new Exception(s"expect JsBoolean, but got $value")

    def show(indent: Int): String =
      val buffer = new StringBuilder
      val cr = if indent > 0 then "\n" else ""  // when no ident, dont crlf

      def show0(value: JsValue, indentString: String): Unit =
        value match
          case JsNull => buffer.append("null")
          case JsBoolean(value) => buffer.append(value)
          case JsNumber(value) => buffer.append(value)
          case JsString(value) => buffer.append(escapedString(value))
          case JsObject(fields) =>
            buffer.append("{").append(cr)
            val pos = buffer.length
            fields.foreach { case (name, value) =>
              if(buffer.length > pos) {
                buffer.append(",").append(cr)
              }
              buffer.append(indentString + " " * indent)
              buffer.append(escapedString(name))
              buffer.append(":")
              show0(value, indentString + " " * indent)
            }
            buffer.append(cr).append(indentString).append("}")
          case JsArray(elements) =>
            buffer.append("[").append(cr)
            val pos = buffer.length
            elements.foreach { elem =>
              if buffer.length > pos then buffer.append(",").append(cr)
              buffer.append( indentString + " " * indent)
              show0(elem, indentString + " " * indent)
            }
            buffer.append(cr).append(indentString).append("]")

      def escapedString(str: String): String =
        val sb = new StringBuilder()
        sb.append('"')
        str.foreach {
          case '\\' => sb.append("\\\\")
          case '"' => sb.append("\\\"")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          // case x if x > 0x100 => sb.append("\\u%04x".format(x.toInt))
          case c => sb.append(c)
        }
        sb.append('"')
        sb.toString()

      show0(value, "")
      buffer.toString


export JsValue.{JsNull, JsBoolean, JsNumber, JsString, JsArray, JsObject}

extension (str:String)
  def parseJson: JsValue = JsValue.parseJson(str)
  def parseJson5: JsValue = JsValue.parseJson5(str)

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def json5 = new Json5Interpolation(sc)

