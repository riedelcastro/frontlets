package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import collection.mutable
import org.scalatest.matchers.MustMatchers

/**
 * @author riedelcastro
 */
object FrontletSpec {
  class Address extends Frontlet {
    val street = StringSlot("street")
    val number = IntSlot("number")
  }

  class Job extends Frontlet {
    val title = StringSlot("title")
    val company = StringSlot("companies")
  }

  class Person extends Frontlet {
    val age = IntSlot("age")
    val address = FrontletSlot("address", () => new Address)
    val hobbies = StringListSlot("hobbies")
    val firstName = StringSlot("firstName")
    val experience = FrontletListSlot("experience",() => new Job)
  }

  class ImmutablePerson extends OuterFrontlet[ImmutablePerson] {
    val age = IntSlot("age")
    val address = FrontletSlot("address", () => new Address)
    val hobbies = StringListSlot("hobbies")
  }
}

class FrontletSpec extends FunSpec with MustMatchers{

  import FrontletSpec._

  describe("A Mutable Frontlet") {
    it("should store primitive values") {
      val person = new Person()
      person.age := 36
      person.age() must be (36)
    }

    it("should store frontlet values") {
      val person = new Person()
      person.address := new Address().street("Broadway").number(1)
      person.address().number() must be (1)
      person.address().street() must be ("Broadway")
    }

    it("should store primitive list values") {
      val person = new Person()
      person.hobbies := Seq("soccer","rap")
      person.hobbies() must  be (Seq("soccer","rap"))
    }

    it("should strong fronlet list values") {
      val person = new Person()
      def job1 = new Job().title("researcher").company("UMass")
      def job2 = new Job().title("lecturer").company("UCL")
      person.experience := Seq(job1,job2)
      person.experience() must be (Seq(job1,job2))
    }


    it("should load and write json strings") {
      val person1 = new Person().age(36).address.create(_.number(1).street("Broadway")).hobbies(Seq("ping-pong"))
      val json1 = person1.toJSON
      val person2 = new Person().setJSON(json1)
      val json2 = person2.toJSON
      json1 must be (json2)
    }
  }

  describe("An immutable Frontlet") {
    it("should create a new frontlet for every change, leaving the current one intact") {
      val person = new ImmutablePerson().age(18)
      val changed = person.age := 36
      person.age() must be (18)
      changed.age() must be (36)
    }
  }



}
