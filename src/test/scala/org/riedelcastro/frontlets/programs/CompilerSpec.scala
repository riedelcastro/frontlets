package org.riedelcastro.frontlets.programs

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.riedelcastro.frontlets.Frontlet

/**
 * @author Sebastian Riedel
 */
class CompilerSpec extends FunSpec with MustMatchers{

  import TermImplicits._

  describe("A compiler") {
    it ("should support integer addition") {
      val i = SimpleVar("i",0)
      val x = SimpleVar("x",0)
      val exe = Compiler.compile(Program(Seq(x := i + i)))
      exe.execute(State(Map(i -> 1)))(x) must be (2)
    }

    it ("should support frontlet slot getter for simple slots") {
      class Person extends Frontlet {
        val age = IntSlot("age")
      }
      val x = SimpleVar("x",0)
      val exe = Compiler.compile(Program(Seq(x := Const(new Person().age(20))(_.age))))
      exe.execute(State.empty)(x) must be (20)
    }

    it ("should support frontlet slot setter for simple slots") {
      class Person extends Frontlet {
        val age = IntSlot("age")
      }
      val x = FrontletVar("x",() => new Person)
      val exe = Compiler.compile(Program(Seq(x := Const(new Person())(_.age,20))))
      exe.execute(State.empty)(x) must be (new Person().age(20))
    }



  }
}
