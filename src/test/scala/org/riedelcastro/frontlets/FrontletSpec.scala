package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import collection.mutable
import org.scalatest.matchers.MustMatchers

/**
 * @author riedelcastro
 */

class FrontletSpec extends FunSpec with MustMatchers{
  class Person extends Frontlet {
    val age = IntSlot("age")
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
      person._map("age") must be (36)

    }
  }

}
