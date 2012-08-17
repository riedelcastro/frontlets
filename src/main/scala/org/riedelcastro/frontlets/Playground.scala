package org.riedelcastro.frontlets

import org.apache.bcel.generic._
import org.apache.bcel.Constants
import tools.nsc.util.ScalaClassLoader.URLClassLoader
import java.net.URL
import scala.collection
import scala.collection
import scala.collection.immutable.HashMap






object Playground {

  class Person extends Frontlet {
    val age = IntSlot("age")
  }

  def compilePrototype2Class(prototype: AbstractFrontlet) = {
    //creates a java class (in bytecode) that can be initialized with a map, and which can represent the
    //data in the prototype
    null
  }

  def main(args: Array[String]) {


    trait Term[+T]
    case class Binding[+T](variable: Var[T], value: T)
    case class Var[+T](name: String) extends Term[T]
    case class Assignment[+T](variable: Var[T], term: Term[T])
    case class Program(assignments: Seq[Assignment[_]])
    case class Const[+T](value: T) extends Term[T]
    case class SlotValue[F <: AbstractFrontlet, T](frontlet: Term[F], slot: F => F#Slot[T]) extends Term[T]

    trait Compilation {
      def execute(bindings: Seq[Binding[_]]): Seq[Binding[_]]
    }

    case class ArrayFrontlet(ints: Array[Int],
                             strings: Array[String],
                             frontlets: Array[ArrayFrontlet])

    case class Path(steps: Array[Int])

    case class Command(command: Int) {
      final def run(stack: ArrayFrontlet, dst: ArrayFrontlet, pointer: Path) {

      }
    }

    def compile(program: Program): Compilation = {

      def evaluate[T](term: Term[T], bindings: Seq[Binding[_]]): T = term match {
        case SlotValue(f, s) => s(evaluate(f, bindings)).value
        case Const(v) => v
        case _ => sys.error("Can't evaluate " + term)
      }
      new Compilation {
        def execute(bindings: Seq[Binding[_]]) = {
          for (assignment <- program.assignments) yield {
            Binding(assignment.variable, evaluate(assignment.term, bindings))
          }
        }
      }
    }



    var result = Var[Int]("result")
    val compilation = compile(Program(Seq(Assignment(result, SlotValue[Person, Int](Const(new Person().age(36)), _.age)))))
    println(compilation.execute(Seq.empty))
    //for (p <- PersonDom) yield new Person().age(p.age() + 5)
  }

}

object Blah {
  def main(args: Array[String]) {
    val b = new Blub
    println(b.i)
    println(b.sq)
  }
}

class Blub(v: Int = 1) {
  var i = 0
  final val k = v

  final def sq = k * k
}