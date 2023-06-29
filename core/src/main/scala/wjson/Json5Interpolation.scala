package wjson

import wjson.*

class Json5Interpolation(sc: StringContext) extends JsonInterpolation(sc) {

  override protected def parse(input: ParserInput): JsValue =
    new Json5Parser(input).parseJsValue()

}
