package wjson.schema.generator

import scala.deriving.Mirror
import scala.quoted.*

object SchemaGenerator:

  inline def generateSchema[T: deriving.Mirror.Of]: Unit = ${ generateSchemaImpl[T] }

  private def generateSchemaImpl[T: Type](using Quotes): Expr[Unit] =

    Expr.summon[Mirror.Of[T]] match
      case Some('{ $m: Mirror.ProductOf[T] }) =>
        genProduct[T]
      case Some('{ $m: Mirror.SumOf[T] {
          type MirroredElemTypes = elemTypes
          type MirroredElemLabels = elemNames
        } }) =>
        genSum[T, elemTypes, elemNames]
      case _ =>
        ???
    '{ () }

  private def genProduct[T: Type](using Quotes): Unit =

    ???

  private def genSum[T: Type, ElemTypes, ElemLabels](using Quotes): Unit =

    ???