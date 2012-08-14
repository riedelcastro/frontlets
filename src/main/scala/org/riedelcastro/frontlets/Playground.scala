package org.riedelcastro.frontlets

/**
 * @author riedelcastro
 */

object Playground {

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

    class Person extends Frontlet {
      val age = IntSlot("age")
    }


    var result = Var[Int]("result")
    val compilation = compile(Program(Seq(Assignment(result, SlotValue[Person, Int](Const(new Person().age(36)), _.age)))))
    println(compilation.execute(Seq.empty))

  }

}

