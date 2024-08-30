package demo

import demo.Aggregator.SUM
import demo.ColumnDef.{CalcColumn, OriginalColumn, RangeColumn}
import wjson.schema.JsonSchema.{*, given}
import wjson.schema.generator.JsonSchemaGenerator
import wjson.*

import java.io.{FileOutputStream, PrintWriter}

type ViewName = String

@description("OLAP Cube")
case class Cube(

                 name: Option[String],

                 @description("source views")
                 @minLength(1)
                 views: List[View],

                 @description("olap dimensions")
                 @minLength(1)
                 dimensions: List[Dimension],

                 @description("olap measures")
                 @minLength(1)
                 measures: List[Measure],

                 viewName: ViewName = "default"

               )

case class Dimension(
                      @description("dimension name")
                      @pattern("""[a-zA-Z]+""")
                      name: String,
                      column: Pointer[Column]
                    )

case class Measure(
                    name: String,
                    column: Pointer[Column],
                    aggregator: Aggregator
                  )

case class View(
                 name: String,
                 columns: List[Column])

case class Column(
                   name: String,
                   columnDef: ColumnDef)

enum ColumnDef:
    case OriginalColumn(name: String)
    case CalcColumn(expr: String)
    case RangeColumn


enum Aggregator:
    case SUM, AVG, MAX, MIN, COUNT, COUNT_DISTINCT

object Demo1:

    def makeCube(): Cube =
        Cube(
            name = None,
            views = List(
                View(name = "orders", columns = List(
                    Column(name = "orderId", columnDef = OriginalColumn("orderId")),
                    Column(name = "orderDate", columnDef = CalcColumn("year(order_date)")),
                    Column(name = "amount", columnDef = RangeColumn )
                ))
            ),
            dimensions = List(
                Dimension(name = "orderId", column = "orderId"),
                Dimension(name = "orderDate", column = "orderDate")
            ),
            measures = List(
                Measure(name = "amount", column = "amount", aggregator = SUM)
            )
        )

    def main(args: Array[String]): Unit =
        val cube = makeCube()

        val expect =
            json5"""
              {
                views: [
                  {
                    name: "orders",
                    columns: [
                      { name: "orderId" },
                      { name: "orderDate" },
                      { name: "amount" }
                    ]
                  }
                ],
                dimensions: [
                  { name: "orderId", column: "orderId" },
                  { name: "orderDate", column: "orderDate" }
                ],
                measures: [
                  { name: "amount", column: "amount", aggregator: "SUM" }
                ]
              }
              """
            //

//        val unmarshaled = expect.convertTo[Cube]
//        println(cube)
//        println(unmarshaled)
//        assert(cube == unmarshaled)

        val schema = JsonSchemaGenerator.of[Cube]
        val out = new PrintWriter(new FileOutputStream("/Users/wangzaixiang/workspaces/wangzaixiang/wjson/demo/demo1.json"))
        out.println(schema)
        out.close()
