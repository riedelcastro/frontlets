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
    val spouse = FrontletSlot("spouse",() => new Person)
  }

  def compilePrototype2Class(prototype: AbstractFrontlet) = {
    //creates a java class (in bytecode) that can be initialized with a map, and which can represent the
    //data in the prototype
    null
  }


  def main(args: Array[String]) {


    trait Term[+T]
    class FrontletTerm[F<:AbstractFrontlet](constructor:() => F) extends Term[F] {
      def set[T](member:F => F#BasicSlot[T],value: Term[T]): FrontletTerm[F] = {
        this
      }
      def setF[T<:AbstractFrontlet](member:F=>F#FrontletSlot[T], value:T=>FrontletTerm[T]):FrontletTerm[F] = {
        this
      }
      def get[T](member:F=> F#BasicSlot[T]):FrontletTerm[F] = {
        this
      }
    }

    implicit def toFrontletTerm[F<:AbstractFrontlet](frontlet:F) = {
      new FrontletTerm[F](() => frontlet)
    }


    implicit def primitiveSlot2FrontletTermBuilder[F<:AbstractFrontlet,T](slot:F#PrimitiveSlot[T]) = {
      new AnyRef {
        def set(value:Term[T]):FrontletTerm[F] = {
          null
        }
      }
    }
    implicit def frontletSlot2FrontletTermBuilder[F<:AbstractFrontlet,T<:AbstractFrontlet](slot:F#FrontletSlot[T]) = {
      new AnyRef {
        def set(value: T=> Term[T]):FrontletTerm[F] = {
          null
        }
      }
    }


    case class Binding[+T](variable: Var[T], value: T)
    case class Var[+T](name: String) extends Term[T]
    case class Assignment[+T](variable: Var[T], term: Term[T])
    case class Program(assignments: Seq[Assignment[_]])
    case class Const[+T](value: T) extends Term[T]
    case class Get[F <: AbstractFrontlet, T](frontlet: Term[F], slot: F => F#Slot[T]) extends Term[T]
    case class Set[F <: AbstractFrontlet, T](frontlet: Term[F], slot: F => F#Slot[T], value:Term[T]) extends Term[F]


    val personTerm = new FrontletTerm(() => new Person)
    personTerm.setF[Person](_.spouse,_.setF[Person](_.spouse,_.set(_.age,Const(5))))
    //person.spouse.set(_.age.set(Const(30))
    val person = new Person
    person.age.set(Const(1))
    person.spouse.set(_.age.set(Const(100)))

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
        case Get(f, s) => s(evaluate(f, bindings)).value
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
    val compilation = compile(Program(Seq(Assignment(result, Get[Person, Int](Const(new Person().age(36)), _.age)))))
    println(compilation.execute(Seq.empty))
    //... yield Create(() => new Person).has(_age,Plus(Var(p,_.age),5)).has(_.spouse, _.age.has(50))
    //... yield new Person.set(_.age,p.get(_.age)).set(_.spouse, _.set(_.age,50))
    //for (p <- PersonsVar) yield new Person().age(p.age() + 5).spouse(new Person().age(50))
    // --> PersonsVar(age:1),ResultType=Array[Person(age,spouse{age}]
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