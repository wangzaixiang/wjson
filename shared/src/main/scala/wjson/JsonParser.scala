/**
 * Copyright (C) 2014 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wjson

import scala.annotation.{switch, tailrec}
import java.lang.{StringBuilder => JStringBuilder}
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.Charset

/**
 * Fast, no-dependency parser for JSON as defined by http://tools.ietf.org/html/rfc4627.
 */
object JsonParser {
  final val EOI = '\uFFFF' // compile-time constant End Of Input
  final val EOS = '\uFFFE' // compile-time constant, End Of Section (StringContext section), EOS means nextArg

  def parse(input: ParserInput): JsValue = new JsonParser(input).parseJsValue()

  class ParsingException(val summary: String, val detail: String = "")
    extends RuntimeException(if (summary.isEmpty) detail else if (detail.isEmpty) summary else summary + ":" + detail)
}

class JsonParser(input: ParserInput, jsonExtensionSupport: Boolean = false) {
  import JsonParser.{ParsingException, EOI, EOS }

  private[this] val sb = new JStringBuilder
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
    def simpleValue(matched: Boolean, value: JsValue) = if (matched) jsValue = value else fail("JSON Value", mark)

    (cursorChar: @switch) match {
      case 'f' => simpleValue(`false`(), JsValue.JsFalse)
      case 'n' => simpleValue(`null`(), JsValue.JsNull)
      case 't' => simpleValue(`true`(), JsValue.JsTrue)
      case '{' => advance(); `object`()
      case '[' => advance(); `array`()
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => `number`()
      case '"' => `string`(); jsValue = if (sb.length == 0) JsValue.JsEmptyString else JsString(sb.toString)
      case '\'' =>
        if(jsonExtensionSupport) {
          single_quoted_string(); jsValue = if(sb.length == 0) JsValue.JsEmptyString else JsString(sb.toString)
        }
        else fail("JSON Value")
      case EOS =>  jsValue = input.currentArgument(); advance(); ws();
      case _ => fail("JSON Value")
    }
  }

  private def `false`() = advance() && ch('a') && ch('l') && ch('s') && ws('e')
  private def `null`() = advance() && ch('u') && ch('l') && ws('l')
  private def `true`() = advance() && ch('r') && ch('u') && ws('e')

  // http://tools.ietf.org/html/rfc4627#section-2.2
  private def `object`(): Unit = {

    @tailrec def members(map: List[(String, JsValue)]): List[(String, JsValue)] = {
      if(jsonExtensionSupport) IDorString()
      else `string`()

      require(':')
      ws()
      val key = sb.toString
      `value`()
      val nextMap = (key, jsValue) :: map
      if (ws(',')) {
        if(jsonExtensionSupport && cursorChar == '}' ) nextMap
        else members(nextMap)
      }
      else nextMap
    }

    ws()
    jsValue = if (cursorChar != '}') {
      var map: List[(String, JsValue)] = Nil
      map = members(map)
      require('}')
      JsObject(map.reverse)
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
        if (ws(',')) values()
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
  private def `number`() = {
    val start = input.cursor
    val startChar = cursorChar
    ch('-')
    `int`()
    `frac`()
    `exp`()
    jsValue =
      if (startChar == '0' && input.cursor - start == 1) JsValue.JsZero
      else
        val num = new String(input.sliceCharArray(start, input.cursor))
        if(num.indexOf('.') == -1) JsNumber(num.toLong)
        else JsNumber(num.toDouble)
    ws()
  }

  private def `int`(): Unit = if (!ch('0')) oneOrMoreDigits()
  private def `frac`(): Unit = if (ch('.')) oneOrMoreDigits()
  private def `exp`(): Unit = if (ch('e') || ch('E')) { ch('-') || ch('+'); oneOrMoreDigits() }

  private def oneOrMoreDigits(): Unit = if (DIGIT()) zeroOrMoreDigits() else fail("DIGIT")
  @tailrec private def zeroOrMoreDigits(): Unit = if (DIGIT()) zeroOrMoreDigits()

  private def DIGIT(): Boolean = cursorChar >= '0' && cursorChar <= '9' && advance()

  private def IDorString(): Unit = {
    if(cursorChar == '"') `string`()
    else if(cursorChar == '\'') single_quoted_string()
    else if(cursorChar == '_' || cursorChar == '$' ||
      (cursorChar >= 'a' && cursorChar <= 'z') || (cursorChar >= 'A' && cursorChar <= 'Z')) ID()
    else fail("id or string")
  }

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

  private def `ID1`(): Boolean = (cursorChar == '_' || cursorChar == '$' ||
    (cursorChar >= 'a' && cursorChar <= 'z') || (cursorChar >= 'A' && cursorChar <= 'Z')) && advance()

  private def `ID2`(): Boolean = (cursorChar == '_' || cursorChar == '$' || (cursorChar >= '0' && cursorChar <= '9') ||
    (cursorChar >= 'a' && cursorChar <= 'z') || (cursorChar >= 'A' && cursorChar <= 'Z')) && advance()

  // http://tools.ietf.org/html/rfc4627#section-2.5
  private def `string`(): Unit = {
    if (cursorChar == '"') cursorChar = input.nextUtf8Char() else fail("'\"'")
    sb.setLength(0)
    while (`char`()) cursorChar = input.nextUtf8Char()
    require('"')
    ws()
  }

  private def single_quoted_string(): Unit = {
    if(cursorChar == '\'') cursorChar = input.nextUtf8Char() else fail("'")
    sb.setLength(0)
    while(sqs_char()) cursorChar = input.nextUtf8Char()
    require('\'')
    ws()
  }

  private def `char`() =
  // simple bloom-filter that quick-matches the most frequent case of characters that are ok to append
  // (it doesn't match control chars, EOI, '"', '?', '\', 'b' and certain higher, non-ASCII chars)
  //    if (((1L << cursorChar) & ((31 - cursorChar) >> 31) & 0x7ffffffbefffffffL) != 0L) appendSB(cursorChar)
    if (((1L << cursorChar) & ((31 - cursorChar) >> 31) & 0x3ffffffbefffffffL) != 0L) appendSB(cursorChar)
    else cursorChar match {
      case '"' | EOI | EOS => false
      case '\\' => advance(); `escaped`()
      case c => (c >= ' ') && appendSB(c)
    }

  // single-quoted-char
  private def sqs_char() =
    cursorChar match {
      case '\'' | EOI | EOS => false
      case '\\' => advance(); `escaped`()
      case c => (c >= ' ') && appendSB(c)
    }

  private def `escaped`() = {
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
    if (((1L << cursorChar) & ((cursorChar - 64) >> 31) & 0x100002600L) != 0L) { advance(); ws() }

  ////////////////////////// HELPERS //////////////////////////

  private def ch(c: Char): Boolean = if (cursorChar == c) { advance(); true } else false
  private def ws(c: Char): Boolean = if (ch(c)) { ws(); true } else false
  private def advance(): Boolean = { cursorChar = input.nextChar(); true }
  private def appendSB(c: Char): Boolean = { sb.append(c); true }
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

trait ParserInput {
  /**
   * Advance the cursor and get the next char.
   * Since the char is required to be a 7-Bit ASCII char no decoding is required.
   */
  def nextChar(): Char

  /**
   * Advance the cursor and get the next char, which could potentially be outside
   * of the 7-Bit ASCII range. Therefore decoding might be required.
   */
  def nextUtf8Char(): Char

  def currentArgument(): JsValue

  def cursor: Int
  //def length: Int
  //def sliceString(start: Int, end: Int): String
  def sliceCharArray(start: Int, end: Int): Array[Char]
  def getLine(index: Int): ParserInput.Line
}

object ParserInput {
  import JsonParser.{EOI, EOS}

  private final val ErrorChar = '\uFFFD' // compile-time constant, universal UTF-8 replacement character '�'

  def apply(string: String): StringBasedParserInput = new StringBasedParserInput(string)
  def apply(chars: Array[Char]): CharArrayBasedParserInput = new CharArrayBasedParserInput(chars)
  def apply(bytes: Array[Byte]): ByteArrayBasedParserInput = new ByteArrayBasedParserInput(bytes)

  def apply(sc: StringContext, args: Seq[JsValue]): ParserInput = new InterpolationParserInput(sc, args)

  case class Line(lineNr: Int, column: Int, text: String)

  abstract class DefaultParserInput extends ParserInput {
    private var _$cursor: Int = -1

    protected def _cursor = _$cursor
    protected def _cursor_=(pos: Int) = _$cursor = pos

    def cursor = _cursor
    def getLine(index: Int): Line = {
      val sb = new java.lang.StringBuilder
      @tailrec def rec(ix: Int, lineStartIx: Int, lineNr: Int): Line = {
        val nc = nextUtf8Char()
        nc match {
          case '\n' if index > ix => sb.setLength(0); rec(ix + 1, ix + 1, lineNr + 1)
          case '\n' | EOI => Line(lineNr, index - lineStartIx + 1, sb.toString)
          case EOS => sb.append("$?"); rec(ix+1, lineStartIx, lineNr)
          case c => sb.append(c); rec(ix + 1, lineStartIx, lineNr)
        }
      }
      val savedCursor = _cursor
      _cursor = -1
      val line = rec(ix = 0, lineStartIx = 0, lineNr = 1)
      _cursor = savedCursor
      line
    }
  }

  private val UTF8 = Charset.forName("UTF-8")

  /**
   * ParserInput reading directly off a byte array which is assumed to contain the UTF-8 encoded representation
   * of the JSON input, without requiring a separate decoding step.
   */
  class ByteArrayBasedParserInput(bytes: Array[Byte]) extends DefaultParserInput {
    private val byteBuffer = ByteBuffer.allocate(4)
    private val charBuffer = CharBuffer.allocate(2)
    private val decoder = UTF8.newDecoder()
    def nextChar() = {
      _cursor += 1
      if (_cursor < bytes.length) (bytes(_cursor) & 0xFF).toChar else EOI
    }
    def currentArgument = throw new IllegalStateException
    def nextUtf8Char() = {
      @tailrec def decode(byte: Byte, remainingBytes: Int): Char = {
        byteBuffer.put(byte)
        if (remainingBytes > 0) {
          _cursor += 1
          if (_cursor < bytes.length) decode(bytes(_cursor), remainingBytes - 1) else ErrorChar
        } else {
          byteBuffer.flip()
          val coderResult = decoder.decode(byteBuffer, charBuffer, false)
          charBuffer.flip()
          val result = if (coderResult.isUnderflow & charBuffer.hasRemaining) charBuffer.get() else ErrorChar
          byteBuffer.clear()
          if (!charBuffer.hasRemaining) charBuffer.clear()
          result
        }
      }

      if (charBuffer.position() > 0) {
        val result = charBuffer.get()
        charBuffer.clear()
        result
      } else {
        _cursor += 1
        if (_cursor < bytes.length) {
          val byte = bytes(_cursor)
          if (byte >= 0) byte.toChar // 7-Bit ASCII
          else if ((byte & 0xE0) == 0xC0) decode(byte, 1) // 2-byte UTF-8 sequence
          else if ((byte & 0xF0) == 0xE0) decode(byte, 2) // 3-byte UTF-8 sequence
          else if ((byte & 0xF8) == 0xF0) decode(byte, 3) // 4-byte UTF-8 sequence
          else ErrorChar
        } else EOI
      }
    }
    def length = bytes.length
    def sliceString(start: Int, end: Int) = new String(bytes, start, end - start, UTF8)
    def sliceCharArray(start: Int, end: Int) =
      UTF8.decode(ByteBuffer.wrap(java.util.Arrays.copyOfRange(bytes, start, end))).array()
  }

  class StringBasedParserInput(string: String) extends DefaultParserInput {
    def nextChar(): Char = {
      _cursor += 1
      if (_cursor < string.length) string.charAt(_cursor) else EOI
    }
    def currentArgument = throw new IllegalStateException
    def nextUtf8Char() = nextChar()
    def length = string.length
    def sliceString(start: Int, end: Int) = string.substring(start, end)
    def sliceCharArray(start: Int, end: Int) = {
      val chars = new Array[Char](end - start)
      string.getChars(start, end, chars, 0)
      chars
    }
  }

  class CharArrayBasedParserInput(chars: Array[Char]) extends DefaultParserInput {
    def nextChar(): Char = {
      _cursor += 1
      if (_cursor < chars.length) chars(_cursor) else EOI
    }
    def currentArgument() = throw new IllegalStateException
    def nextUtf8Char() = nextChar()
    def length = chars.length
    def sliceString(start: Int, end: Int) = new String(chars, start, end - start)
    def sliceCharArray(start: Int, end: Int) = java.util.Arrays.copyOfRange(chars, start, end)
  }

  class InterpolationParserInput(sc: StringContext, args: Seq[JsValue]) extends DefaultParserInput {
    private var _gCursor = -1

    private var _sectionIndex = 0 // 0 .. sc.parts.length-1
    private var _sectionContent: String = sc.parts(0)
    private var _sectionCursor = -1 // the current cursor inside current section
    private var _sectionStartCursor = 0

    def switchToNextSection(): Unit = {
      _sectionIndex += 1
      _sectionContent = sc.parts(_sectionIndex)
      _sectionStartCursor = _cursor
      _sectionCursor = 0
    }

    override def _cursor = _gCursor

    // only used in error recover
    override def _cursor_=(pos: Int) = {
      _gCursor = -1
      _sectionIndex = 0
      _sectionContent = sc.parts(0)
      _sectionCursor = -1
      _sectionStartCursor = 0

      move(pos)

      @tailrec
      def move(steps: Int): Unit = {
        if(steps >= 0) {
          nextChar()
          move(steps-1)
        }
      }
    }

    final def nextChar(): Char = {
      _gCursor += 1
      _sectionCursor += 1

      if(_sectionCursor > _sectionContent.length && _sectionIndex >= sc.parts.length - 1) {
        EOI
      }
      else {
        if (_sectionCursor > _sectionContent.length) { // move to next section
          switchToNextSection()
        }

        // _sectionCursor [0 .. length-1] -> return char
        //  length -> EOS|EOI
        if (_sectionCursor < _sectionContent.length) _sectionContent.charAt(_sectionCursor)
        else if (_sectionIndex < sc.parts.length - 1) EOS
        else EOI
      }
    }

    def currentArgument(): JsValue = args(_sectionIndex)

    override def nextUtf8Char(): Char = nextChar()

    // start and end must inside 1 section
    override def sliceCharArray(start: Int, end: Int): Array[Char] = {
      val chars = new Array[Char](end - start)
      _sectionContent.getChars(start - _sectionStartCursor, end - _sectionStartCursor, chars, 0)
      chars
    }
  }

}