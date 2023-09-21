package wjson_test

import org.scalatest.funsuite.AnyFunSuite
import wjson.JsValueMapper

object CubeModel:

  opaque type DimensionName = String
  opaque type MeasureName = String

  case class Dimension(name: DimensionName, alias: String, desc: String)

  case class Measure(name: MeasureName, alias: String, desc: String)

  case class Cube(dimensions: Seq[Dimension], measures: Seq[Measure])

  inline def dimensionName(name: String): DimensionName = name
  inline def measureName(name: String): MeasureName = name


class TestOpaque extends AnyFunSuite {

  test("opaque types") {

    import CubeModel.*
    import wjson.*

    val cube = Cube(
      Seq(
        Dimension(dimensionName("dim1"), "Dim 1", "Dimension 1"),
        Dimension(dimensionName("dim2"), "Dim 2", "Dimension 2")
      ),
      Seq(
        Measure(measureName("measure1"), "Measure 1", "Measure 1"),
        Measure(measureName("measure2"), "Measure 2", "Measure 2")
      )
    )

    val js = cube.toJson
    println(js.showPretty)

  }

}
