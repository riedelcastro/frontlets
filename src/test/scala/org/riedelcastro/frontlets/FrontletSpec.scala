package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import collection.mutable
import org.scalatest.matchers.MustMatchers

/**
 * @author riedelcastro
 */

class FrontletSpec extends FunSpec with MustMatchers{
  class Address extends Frontlet {
    val street = StringSlot("street")
    val number = IntSlot("number")
  }
  class Person extends Frontlet {
    val age = IntSlot("age")
    val address = FrontletSlot("address", () => new Address)
    val hobbies = StringListSlot("hobbies")
  }

  describe("A Frontlet") {
    it("should wrap around a map and retrieve the map's values in a typed manner") {
      val map = new mutable.HashMap[String,Any]
      map("age") = 36
      val person = new Person().setMap(map)
      person.age() must be (36)
    }
    it("should store the value assigned to a slot in the underlying map") {
      val person = new Person()
      person.age := 36
      person.asMap("age") must be (36)
    }
    it("should load and write json strings") {
      val person1 = new Person().age(36).address.create(_.number(1).street("Broadway")).hobbies(Seq("ping-pong"))
      val json1 = person1.toJSON
      val person2 = new Person().setJSON(json1)
      val json2 = person2.toJSON
      json1 must be (json2)
    }
  }

}
