//package wjson.macros
//
//import wjson.JsValueMapper
//
//object TupleMacro:
//
//    inline def [T <: Tuple](t: T): JsValueMapper[T] =
//        ${ TupleMacroImpl.toJsArray('t) }
//
//    def [T <: Tuple](t: T) toJsArray: JsValueMapper[T] =
//        JsValueMapper(t.productIterator.map(JsValueMapper(_)).toSeq)