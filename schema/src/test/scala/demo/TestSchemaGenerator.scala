package demo

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsValue.JsBoolean
import wjson.JsValueMapper
import wjson.schema.generator.JsonSchemaGenerator
import wjson.schema.{JsonSchema => js}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.io.Source
import wjson.*

@js.description("a primary bean")
case class PrimaryBean
(
  @js.description("a bool field")
  bool: Boolean = false,
  byte: Byte = 0,
  short: Short = 1,
  int: Int = 2,
  long: Long = 3,
  float: Float = 4,
  double: Double = 5,
  string: String = "6",
  scalaBigDecimal: BigDecimal = BigDecimal(7),
  javaBigDecimal: java.math.BigDecimal = new java.math.BigDecimal(8),
  scalaBigInt: BigInt = BigInt(9),
  javaBigInt: java.math.BigInteger = new java.math.BigInteger("10"),

  _bool: JsBoolean = JsBoolean(false),
  _string: wjson.JsValue.JsString = wjson.JsValue.JsString("11"),
  _number: wjson.JsValue.JsNumber = wjson.JsValue.JsNumber(12),
  _array: wjson.JsValue.JsArray = wjson.JsValue.JsArray(Nil),
  _object: wjson.JsValue.JsObject = wjson.JsValue.JsObject(Nil),
)

case class OptionPrimaryBean
(
  bool: Option[Boolean],
  byte: Option[Byte],
  short: Option[Short],
  int: Option[Int],
  long: Option[Long],
  float: Option[Float],
  double: Option[Double],
  string: Option[String],
  scalaBigDecimal: Option[BigDecimal],
  javaBigDecimal: Option[java.math.BigDecimal],
  scalaBigInt: Option[BigInt],
  javaBigInt: Option[java.math.BigInteger],

  _bool: Option[JsBoolean],
  _string: Option[wjson.JsValue.JsString],
  _number: Option[wjson.JsValue.JsNumber],
  _array: Option[wjson.JsValue.JsArray],
  _object: Option[wjson.JsValue.JsObject],
)

case class CollectionBean
(
  array: Array[Int],
  list: List[String],
  seq:  Seq[Float],
  set:  Set[Double],
  sorted: SortedSet[Long],
  map1: Map[String, PrimaryBean],
  map2: Map[PrimaryBean, String],
  map3: SortedMap[String, PrimaryBean]
)

@js.toplevel
case class User //123
(
  name: String,
  age: Int,
  gender: Boolean
)

enum Color:
    case Red, Green, Blue
    case Mixed(r: Int, g: Int, b: Int, alpha: Int)

class TestSchemaGenerator extends AnyFunSuite {

    test("user"){
        val schema = JsonSchemaGenerator.of[User].parseJson
        println(schema.showPretty)
        val expect = Source.fromResource("demo/user.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

    test("primary"){
        val schema = JsonSchemaGenerator.of[PrimaryBean].parseJson
        val expect = Source.fromResource("demo/primary.schema.json5").mkString.parseJson
        println(schema.showPretty)
        assert(schema == expect)
    }

    test("optional"){
        val schema = JsonSchemaGenerator.of[OptionPrimaryBean].parseJson
        val expect = Source.fromResource("demo/optional.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

    test("collection bean"){
        val schema = JsonSchemaGenerator.of[CollectionBean].parseJson
        val expect = Source.fromResource("demo/collection.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

    test("enum") {
        val schema = JsonSchemaGenerator.of[Color].parseJson
        val expect = Source.fromResource("demo/color.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

    test("or type 1") { //
        type type1 = String | Int | Boolean

        val schema = JsonSchemaGenerator.of[type1].parseJson
        val expect = Source.fromResource("demo/or_type1.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

    test("or type 2") { // 123
        case class Bean(name: String | Null)

        val schema = JsonSchemaGenerator.of[Bean].parseJson
        val expect = Source.fromResource("demo/or_type2.schema.json5").mkString.parseJson
        assert(schema == expect)

    }

    test("or type 3"){ //12
        type T1 = String | Int | User | Null
        type T2 = Option[User] | Option[String] | Int
        type T3 = PrimaryBean | OptionPrimaryBean | List[PrimaryBean]

        @js.toplevel
        case class Bean(name: T1, other: T2, tags: List[T3]) derives JsValueMapper; //8

        val schema = JsonSchemaGenerator.of[Bean].parseJson
        println(schema.showPretty)
        val expect = Source.fromResource("demo/or_type3.schema.json5").mkString.parseJson
        assert(schema == expect)
    }

}
