package wjson

import wjson.JsValue.JsNumber
import wjson.{*, given}

import scala.annotation.{switch, tailrec}

/** JSON5 is a superset of JSON, the specification can be found at: http://json5.org/
 * the main differences are:
 * - single quoted strings are allowed
 * - comments are allowed
 * - trailing commas are allowed
 * - trailing commas are allowed in objects and arrays
 * - numbers can be hexadecimal
 * - numbers can have a leading or trailing decimal point
 */
class Json5Parser(input: ParserInput) {
  import JsonParser.{ParsingException, EOI, EOS }

  private[this] val sb = new java.lang.StringBuilder
  private[this] var cursorChar: Char = input.nextChar()
  private[this] var jsValue: JsValue = _

  def parseJsValue(): JsValue = {
    ws()
    `value`()
    require(EOI)
    jsValue
  }


  ////////////////////// GRAMMAR ////////////////////////


  // http://tools.ietf.org/html/rfc4627#section-2.1
  private def `value`(): Unit = {
    val mark = input.cursor
    inline def simpleValue(matched: Boolean, value: JsValue) =
      if (matched) jsValue = value else fail("JSON Value", mark)

    (cursorChar: @switch) match {
      case 'f' => simpleValue(`false`(), JsValue.JsFalse)
      case 'n' => simpleValue(`null`(), JsValue.JsNull)
      case 't' => simpleValue(`true`(), JsValue.JsTrue)
      case '{' => advance(); `object`()
      case '[' => advance(); `array`()
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' | '.' => `number`()
      case '"' => double_quoted_string(); jsValue = if (sb.length == 0) JsValue.JsEmptyString else JsString(sb.toString)
      case '\'' => single_quoted_string(); jsValue = if (sb.length == 0) JsValue.JsEmptyString else JsString(sb.toString)
      case EOS =>  jsValue = input.currentArgument(); advance(); ws();
      case _ => fail("JSON Value")
    }
  }

  private def `false`() = advance() && ch('a') && ch('l') && ch('s') && ws('e')
  private def `null`() = advance() && ch('u') && ch('l') && ws('l')
  private def `true`() = advance() && ch('r') && ch('u') && ws('e')

  // http://tools.ietf.org/html/rfc4627#section-2.2
  private def `object`(): Unit = {  // already consumed the '{' character

    @tailrec def members(map: List[(String, JsValue)]): List[(String, JsValue)] = {
      IDorString(); ws(); require(':'); ws()
      val key = sb.toString

      `value`()
      ws()

      val map2 = (key, jsValue) :: map

      if cursorChar == '}' then map2
      else if cursorChar == ',' then
        ws(',')
        if cursorChar == '}' then map2
        else members(map2)
      else map2
    }

    ws()
    jsValue = if (cursorChar != '}') {
      var map: List[(String, JsValue)] = Nil
      map = members(map)
      require('}')
      new JsObject(map.reverse)
    } else {
      advance()
      JsValue.JsEmptyObject
    }
    ws()
  }

  // http://tools.ietf.org/html/rfc4627#section-2.3
  private def `array`(): Unit = {
    ws()
    jsValue = if (cursorChar != ']') {
      val list = Vector.newBuilder[JsValue]
      @tailrec def values(): Unit = {
        `value`()
        list += jsValue
        ws()
        if cursorChar == ',' then
          advance(); ws();
          if cursorChar == ']' then ()
          else values();
        else if cursorChar != ']' then
          fail("require ',' or ']'")
        else ()
      }
      values()
      require(']')
      JsArray(list.result().toList)
    } else {
      advance()
      JsValue.JsEmptyArray
    }
    ws()
  }

  // http://tools.ietf.org/html/rfc4627#section-2.4
  private def `number`(): Unit = {
    val start = input.cursor
    val startChar = cursorChar

    val sign =
      if cursorChar == '-' then { advance(); -1 }
      else if cursorChar == '+' then { advance(); 1 }
      else 1

    val i: Long = `int`()
    val f: Double = `frac`()
    val e: Long = `exp`()

    jsValue =
      if (startChar == '0' && input.cursor - start == 1) JsValue.JsZero
      else
        if f == 0 && e == 0 then JsNumber(sign * i)
        else if f == 0 then // i, e
          JsNumber( sign * i.toDouble * Math.pow(10, e) )
        else // f != 0
          JsNumber( sign * (i.toDouble + f) * Math.pow(10, e) )
    ws()
  }

  private def `int`(): Long =
    if ch('0') then
      if cursorChar == 'x' || cursorChar == 'X' then
        advance()
        oneOrMoreHexDigits(0)
      else 0
    else if cursorChar >= '1' && cursorChar <= '9' then
      oneOrMoreDigits(0)
    else 0

  private def `frac`(): Double =
    if (ch('.')) then
      val p0 = input.cursor
      val num  = zeroOrMoreDigits(0)
      val p1 = input.cursor
      num.toDouble / Math.pow(10, p1 - p0)
    else 0
  private def `exp`(): Long =
    if ch('e') || ch('E') then
      ch('-') || ch('+')
      oneOrMoreDigits(0)
    else 0


  private def oneOrMoreDigits(num: Long): Long =
    if cursorChar >= '0' && cursorChar <= '9' then
      val char = cursorChar
      advance()
      zeroOrMoreDigits(num * 10 + (char - '0'))
    else fail("DIGIT")

  @tailrec private def zeroOrMoreDigits(num: Long): Long =
    if cursorChar >= '0' && cursorChar <= '9' then
      val char = cursorChar
      advance()
      zeroOrMoreDigits(num * 10 + (char - '0'))
    else num

  private def oneOrMoreHexDigits(num: Long): Long =

    if cursorChar >= '0' && cursorChar <= '9' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - '0')  )
    else if cursorChar >= 'a' && cursorChar <= 'f' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - 'a') + 10 )
    else if cursorChar >= 'A' && cursorChar <= 'F' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - 'A') + 10)
    else fail("HexDigit")


  @tailrec
  private def zeroOrMoreHexDigits(num: Long): Long =
    if cursorChar >= '0' && cursorChar <= '9' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - '0') )
    else if cursorChar >= 'a' && cursorChar <= 'f' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - 'a') + 10 )
    else if cursorChar >= 'A' && cursorChar <= 'F' then
      val char = cursorChar
      advance()
      zeroOrMoreHexDigits( num * 16 + (char - 'A') + 10 )
    else num

  private def DIGIT(): Boolean = cursorChar >= '0' && cursorChar <= '9' && advance()
  private def HexDigit(): Boolean = ((cursorChar >= '0' && cursorChar <= '9') ||
    (cursorChar >= 'a' && cursorChar <= 'f') ||
    (cursorChar >= 'A' && cursorChar <= 'F')) && advance()

  private def `ID`(): Unit = {
    val start = input.cursor
    if(ID1()) {
      var cont = true
      while(cont) {
        cont = ID2()
      }

      sb.setLength(0)
      sb.append(input.sliceCharArray(start, input.cursor))
    }
    else fail("ID")
  }

  private def IDorString(): Unit =
    if cursorChar == '\"' || cursorChar == '\'' then
      `string`()
    else ID()

  private def `ID1`(): Boolean = (cursorChar == '_' || cursorChar == '$' ||
    (cursorChar >= 'a' && cursorChar <= 'z') || (cursorChar >= 'A' && cursorChar <= 'Z')) && advance()

  private def `ID2`(): Boolean = (cursorChar == '_' || cursorChar == '$' || (cursorChar >= '0' && cursorChar <= '9') ||
    (cursorChar >= 'a' && cursorChar <= 'z') || (cursorChar >= 'A' && cursorChar <= 'Z')) && advance()

  // http://tools.ietf.org/html/rfc4627#section-2.5
  private def `string`(): Unit = {
    if cursorChar == '"' then double_quoted_string()
    else if cursorChar == '\'' then single_quoted_string()
    else fail("require '\"' or '''");
  }

  private def double_quoted_string(): Unit = {
    if (cursorChar == '"') cursorChar = input.nextUtf8Char() else fail("'\"'")
    sb.setLength(0)
    while (dqs_char()) cursorChar = input.nextUtf8Char()
    require('"')
    ws()
  }
  private def single_quoted_string(): Unit = {
    if (cursorChar == '\'') cursorChar = input.nextUtf8Char() else fail("'")
    sb.setLength(0)
    while (sqs_char()) cursorChar = input.nextUtf8Char()
    require('\'')
    ws()
  }

  private def sqs_char(): Boolean =
    cursorChar match {
      case '\'' | EOI | EOS => false
      case '\\' => advance();
        if cursorChar == '\r' then advance() && ch('\n') && appendSB('\n') && sqs_char()
        else if cursorChar == '\n' then advance() && appendSB('\n') && sqs_char()
        else `escaped`()
      case c => (c >= ' ') && appendSB(c)
    }


  private def dqs_char(): Boolean =
  // simple bloom-filter that quick-matches the most frequent case of characters that are ok to append
  // (it doesn't match control chars, EOI, '"', '?', '\', 'b' and certain higher, non-ASCII chars)
  //    if (((1L << cursorChar) & ((31 - cursorChar) >> 31) & 0x7ffffffbefffffffL) != 0L) appendSB(cursorChar)
    // if (((1L << cursorChar) & ((31 - cursorChar) >> 31) & 0x3ffffffbefffffffL) != 0L) appendSB(cursorChar)
    cursorChar match {
      case '"' | EOI | EOS => false
      case '\\' =>
        advance()
        if cursorChar == '\r' then advance() && ch('\n') && appendSB('\n') && dqs_char()  // multiline string
        else if cursorChar == '\n' then advance() && appendSB('\n') && dqs_char() // multiline string
        else `escaped`()
      case c => (c >= ' ') && appendSB(c)
    }

  private def `escaped`(): Boolean = {
    def hexValue(c: Char): Int =
      if ('0' <= c && c <= '9') c - '0'
      else if ('a' <= c && c <= 'f') c - 87
      else if ('A' <= c && c <= 'F') c - 55
      else fail("hex digit")
    def unicode() = {
      var value = hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      appendSB(value.toChar)
    }
    (cursorChar: @switch) match {
      case '"' | '/' | '\\' | '\'' => appendSB(cursorChar)
      case 'b' => appendSB('\b')
      case 'f' => appendSB('\f')
      case 'n' => appendSB('\n')
      case 'r' => appendSB('\r')
      case 't' => appendSB('\t')
      case 'u' => advance(); unicode()
      case _ => fail("JSON escape sequence")
    }
  }

  @tailrec private def ws(): Unit =
  // fast test whether cursorChar is one of " \n\r\t"
    if ch('/') then comments()
    if (((1L << cursorChar) & ((cursorChar - 64) >> 31) & 0x100002600L) != 0L) { advance(); ws() }

  def comments(): Unit =
    if ch('/') then { while (cursorChar != '\n' && cursorChar != EOI) advance() }
    else if ch('*') then
      var cont = true
      while (cont) {
        if (cursorChar == '*') {
          advance()
          if (cursorChar == '/') { cont = false; advance() }
        } else advance()
      }
    else fail("comment")

  private inline def ch(c: Char): Boolean = if (cursorChar == c) { advance(); true } else false
  private inline def ws(c: Char): Boolean = if (ch(c)) { ws(); true } else false
  private inline def advance(): Boolean = { cursorChar = input.nextChar(); true }
  private inline def appendSB(c: Char): Boolean = { sb.append(c); true }
  private def require(c: Char): Unit = if (!ch(c)) fail(s"'$c'")

  private def fail(target: String, cursor: Int = input.cursor, errorChar: Char = cursorChar): Nothing = {
    val ParserInput.Line(lineNr, col, text) = input.getLine(cursor)
    val summary = {
      val unexpected =
        if (errorChar != EOI) {
          val c = if (Character.isISOControl(errorChar)) "\\u%04x" format errorChar.toInt else errorChar.toString
          s"character '$c'"
        } else "end-of-input"
      val expected = if (target != "'\uFFFF'") target else "end-of-input"
      s"Unexpected $unexpected at input index $cursor (line $lineNr, position $col), expected $expected"
    }
    val detail = {
      val sanitizedText = text.map(c => if (Character.isISOControl(c)) '?' else c)
      s"\n$sanitizedText\n${" " * (col-1)}^\n"
    }
    throw new ParsingException(summary, detail)
  }
}
