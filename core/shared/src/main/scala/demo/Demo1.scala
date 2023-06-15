package demo

import wjson.{*, given}

// 在 derived generate code 中，如果出现了递归，则报错
//case class User3(name: String, age: Int, languages: List[List[User3]]) derives JsValueMapper // OK
//case class User3(name: String, age: Int, languages: List[User3]) derives JsValueMapper // OK
//case class User3(name: String, age: Int, languages: User3) derives JsValueMapper // Not OK

case class User3(name: String, age: Int, languages: User3); given JsValueMapper[User3] = JsValueMapper.derived[User3] // NOT OK
//case class User3(name: String, age: Int, languages: User3); // but JsValueMapper.derived[User3] called in other place // OK



//case class Family(mother: User3, father: User3, children: List[User3], relate: Family)  derives JsValueMapper
//object Family:
//  given a: JsValueMapper[Family] = JsValueMapper.derived[Family] // 4
//end Family


object Demo1 {

  def main(args: Array[String]): Unit = {
//    case class User1(name: String, age: Int, languages: List[String]) derives JsValueMapper
//    case class User2(name: String, age: Int, languages: collection.immutable.List[String]) derives JsValueMapper

//    val x = JsValueMapper.derived[User3]
//
    // Problem when use in derived generator
//
//    val x2 = summon[JsValueMapper[Family]]

//    val a = {
//      lazy val mapper_1: wjson.JsValueMapper[Family] = {
//        final class $anon() extends wjson.JsValueMapper[Family] {
//          def fromJson(json: wjson.JsValue): Family = {
//            val jso: wjson.JsValue$package.JsObject = json.asInstanceOf[wjson.JsValue$package.JsObject]
//            val x: User3 = wjson.ADTMappingMacro.caseFieldGet[User3](jso, "mother")(User3.derived$JsValueMapper)
//            val `x₂`: User3 = wjson.ADTMappingMacro.caseFieldGet[User3](jso, "father")(User3.derived$JsValueMapper)
//            val `x₃`: scala.collection.immutable.List[scala.collection.immutable.List[User3]] = wjson.ADTMappingMacro.caseFieldGet[scala.collection.immutable.List[scala.collection.immutable.List[User3]]](jso, "children")(
//              wjson.JsValueMapper.given_JsValueMapper_List[scala.collection.immutable.List[User3]](wjson.JsValueMapper.given_JsValueMapper_List[User3](User3.derived$JsValueMapper)))
//            val `x₄`: Family = wjson.ADTMappingMacro.caseFieldGet[Family](jso, "relate", null)(mapper_1)
//            new Family(x, `x₂`, `x₃`, `x₄`)
//          }
//
//          def toJson(value: Family): wjson.JsValue = {
//            val THIS: wjson.JsValueMapper[Family] = this
//            wjson.JsValue$package.JsObject(
//              Seq( Tuple2.apply[java.lang.String, wjson.JsValue]("mother", if (value.mother.==(null)) wjson.JsValue$package.JsNull else User3.derived$JsValueMapper.toJson(value.mother)),
//              Tuple2.apply[java.lang.String, wjson.JsValue]("father", if (value.father.==(null)) wjson.JsValue$package.JsNull else User3.derived$JsValueMapper.toJson(value.father)),
//              Tuple2.apply[java.lang.String, wjson.JsValue]("children", if (value.children.==(null)) wjson.JsValue$package.JsNull else wjson.JsValueMapper.given_JsValueMapper_List[scala.collection.immutable.List[User3]](wjson.JsValueMapper.given_JsValueMapper_List[User3](User3.derived$JsValueMapper)).toJson(value.children)),
//              Tuple2.apply[java.lang.String, wjson.JsValue]("relate", if (value.relate.==(null)) wjson.JsValue$package.JsNull else mapper_1.toJson(value.relate)) )
//                .filter(((_$1: scala.Tuple2[scala.Predef.String, wjson.JsValue]) => _$1._2.!=(wjson.JsValue$package.JsNull))): _*)
//          }
//        }
//
//        (new $anon(): wjson.JsValueMapper[Family])
//      }
//      mapper_1
//    }


  }


}
