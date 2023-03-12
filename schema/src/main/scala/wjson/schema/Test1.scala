package wjson.schema

import scala.quoted.*
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}

object Test1:

  class MyInspector extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect._
      tastys.foreach { tasty =>
        println(tasty.toString)
        val q = tasty.quotes
        val ast = tasty.ast
        val path = tasty.path
        println(s"ast: path=$path")
        println("ast: " + ast.show(using Printer.TreeCode))

        // package
        val pkg = Symbol.requiredPackage("wjson.schema.test")
        val tp = pkg.termRef

        pkg.declarations.foreach { decl =>
          println("decl: " + decl)
          println("\tdesc.isClassDef: " + decl.isClassDef)
          println("\tdesc.isType: " + decl.isType)
          println("\tdesc.isTerm: " + decl.isTerm)

          if (decl.isType) {
            val tp = decl.typeRef
            println("\t" + Console.BLUE + "typeRef: " + tp + Console.RESET)
          }
          else if (decl.isTerm) {
            val tp = decl.termRef
            println("\t" + Console.BLUE + "termRef: " + tp + Console.RESET)
          }

          if (decl.isClassDef) then
            val termRef = decl.termRef
            val termSym1 = termRef.termSymbol
            val typeSym1 =
              try termRef.typeSymbol
              catch
                case e: Throwable => null

            val typeRef = decl.typeRef
            val termSym2 =
              try typeRef.termSymbol
              catch
                case e => null
            val typeSym2 = typeRef.typeSymbol

            val tree = try decl.tree catch case e => null

            def format(x: Any): String = x match
              case null => Console.RED + "null" + Console.RESET
              case x if x == Symbol.noSymbol => Console.YELLOW_B + "noSymbol" + Console.RESET
              case x => x.toString

            println("\ttermRef: " + format(termRef))
            println("\ttermRef.termSymbol: " + format(termSym1))
            println("\ttermRef.typeSymbol: " + format(typeSym1))
            println("\ttypeRef: " + format(typeRef))
            println("\ttypeRef.termSymbol: " + format(termSym2))
            println("\ttypeRef.typeSymbol: " + format(typeSym2))
            println("\ttree: " + format(tree))

          //            val flags = decl.flags
          //            if( tp2 == NoTy)
          //            val tree = decl.tree
          //            println("flags = " + flags)
          //            println("class tree: " + tree.show(using Printer.TreeCode))
        }
        //
      }

    def dumpTest1(using Quotes)(ast: quotes.reflect.Tree): Unit =
      import quotes.reflect.*

      ast match
        case PackageClause(pid, stats) =>
          stats(1) match
            case ClassDef(name, constr, parents, derived, body) =>
              println("name: " + name)
              println("constr: " + constr)
              println("parents: " + parents)
              println("derived: " + derived)
              body(1) match
                case DefDef(name, params, tpt, rhs) =>
                  println("name: " + name)
                  println("tpt: " + tpt)
                  rhs.get match
                    case Block(stats, expr) =>
                      println("stats: " + stats)
                      stats.foreach {
                        case ValDef(name, tpt, rhs) =>
                          println("\tname: " + name)
                          println("\ttpt: " + tpt)
                          println("\trhs: " + rhs)

                          val tp = tpt.tpe
                          println("\ttype: " + tp.show(using Printer.TypeReprStructure))
                          println()
                      }
                      println("expr: " + expr)
                case _ =>
                  println("not a def")


  def main(args: Array[String]): Unit = {
    TastyInspector.inspectTastyFiles(args.toList)(new MyInspector)
  }

