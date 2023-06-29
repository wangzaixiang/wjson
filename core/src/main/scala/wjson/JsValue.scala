package wjson

import wjson.JsValue.{JsArray, JsNumber}

import scala.collection.{IterableOps, SortedMap, SortedSet, mutable}
import scala.reflect.ClassTag

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


object JsValue:
  val JsTrue: JsBoolean = JsBoolean(true)
  val JsFalse: JsBoolean = JsBoolean(false)
  val JsZero: JsNumber = JsNumber(0L)
  val JsEmptyString: JsString = JsString("")
  val JsEmptyObject: JsObject = JsObject()
  val JsEmptyArray: JsArray = JsArray()

  def parse(str: String): JsValue = JsonParser.parse(ParserInput(str))
  def JsObject(fields: (String, JsValue)*): JsObject = JsObject(fields)
  def JsArray(elements: JsValue*): JsArray = JsArray(elements)
  def JsNumber(value: Int) = new JsNumber(value.toLong)


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

    def ++(other: JsObject): JsObject = merge(other.fields:_*)
    def ++(other: Seq[(String, JsValue)]): JsObject = merge(other:_*)

    def +(kv: (String, JsValue)): JsObject =
      if value.fields.exists(_._1 == kv._1) then
        JsObject(value.fields.filterNot(_._1 == kv._1) :+ kv)
      else JsObject(value.fields :+ kv)

  extension (value: JsValue)
    def show: String = show(0, -1)
    def showPretty: String = show(2, 100)

    def show(indent: Int = 2, margin: Int = 100): String =
      val buffer = new StringBuilder

      def show0(value: JsValue, indentString: String): Unit =
        value match
          case JsNull => buffer.append("null")
          case JsBoolean(value) => buffer.append(value)
          case JsNumber(value) => buffer.append(value)
          case JsString(value) => buffer.append(escapedString(value))
          case JsObject(fields) =>
            buffer.append("{\n")
            fields.foreach { case (name, value) =>
              buffer.append(indentString + " " * indent)
              buffer.append(escapedString(name))
              buffer.append(":")
              show0(value, indentString + " " * indent)
              buffer.append(",\n")
            }
            if(buffer.endsWith(",\n")){
              buffer.delete(buffer.length() - 2, buffer.length()-1)
            }
            buffer.append(indentString)
            buffer.append("}")
          case JsArray(elements) =>
            buffer.append("[\n")
            elements.foreach { elem =>
              buffer.append( indentString + " " * indent)
              show0(elem, indentString + " " * indent)
              buffer.append(",\n")
            }
            if(buffer.endsWith(",\n")){
              buffer.delete(buffer.length() - 2, buffer.length()-1)
            }
            buffer.append(indentString).append("]")

      def escapedString(str: String): String =
        val sb = new StringBuilder()
        sb.append('"')
        str.foreach { c =>
          c match
            case '\\' => sb.append("\\\\")
            case '"' => sb.append("\\\"")
            case '\b' => sb.append("\\b")
            case '\f' => sb.append("\\f")
            case '\n' => sb.append("\\n")
            case '\r' => sb.append("\\r")
            case '\t' => sb.append("\\t")
            // case x if x > 0x100 => sb.append("\\u%04x".format(x.toInt))
            case _ => sb.append(c)
        }
        sb.append('"')
        sb.toString()

      show0(value, "")
      buffer.toString


export JsValue.{JsNull, JsBoolean, JsNumber, JsString, JsArray, JsObject}

extension (str:String)

  def parseJson: JsValue = JsValue.parse(str)

extension (sc: StringContext)
  def json = new JsonInterpolation(sc)
  def json5 = new Json5Interpolation(sc)

