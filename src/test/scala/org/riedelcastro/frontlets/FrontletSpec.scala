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

    it("should store fronlet list values") {
      val person = new Person()
      def job1 = new Job().title("researcher").company("UMass")
      def job2 = new Job().title("lecturer").company("UCL")
      person.experience := Seq(job1,job2)
      person.experience() must be (Seq(job1,job2))
    }

    it("should map list values") {
      val person = new Person()
      def job1 = new Job().title("researcher").company("UMass")
      def job2 = new Job().title("lecturer").company("UCL")
      person.experience := Seq(job1,job2)
      person.experience.map(_.title("worker")).experience() must be (Seq(job1.title("worker"),job2.title("worker")))
    }

    it("should access frontlets in a sequence using a position-based constructor") {
      class Item extends Frontlet {
        val index = IntSlot("index")
        val container = TransientSlot[Container]("container")
      }
      class Container extends Frontlet {
        val items = FrontletSeqSlot("items", i => new Item().index(i).container(this))
      }

      val container = new Container().items(Seq(new Item,new Item))
      val items = container.items()
      for ((item,index) <- items.zipWithIndex) {
        item.index() must be (index)
        item.container() must be (container)
      }
    }

    it("should load and write json strings") {
      val person1 = new Person().age(36).address(_.number(1).street("Broadway")).hobbies(Seq("ping-pong"))
      val json1 = person1.toJSON
      json1 must be ("""{"hobbies":["ping-pong"],"age":36,"address":{"street":"Broadway","number":1}}""")
      val person2 = new Person().setJSON(json1)
      val json2 = person2.toJSON
      json1 must be (json2)
    }

    it("should provide access to lists of lists of lists") {
      class Container extends Frontlet {
        val list = GenericListSlot("list", Seq(Seq(Seq(1.0))))
      }
      val c = new Container().setJSON("""{"list": [[[1.0,2.0]]] }""")
      c.list() must be (Seq(Seq(Seq(1.0,2.0))))
    }

    it("should provide qualified paths to its slot") {
      import FrontletImplicits._
      val person = new Person()
      val path = person.address / (_.street)
      path.toPathString must be ("address.street")
    }

  }

  describe("An immutable Frontlet") {
    it("should create a new frontlet for every change, leaving the current one intact") {
      val person = new ImmutablePerson().age(18)
      val changed = person.age := 36
      person.age() must be (18)
      changed.age() must be (36)
    }

    it("should support nested changes") {
      val person = new ImmutablePerson().age(18).address.create(_.street("Broadway"))
      val changed = person.address(_.street("Brick Lane"))
      changed must be (new ImmutablePerson().age(18).address.create(_.street("Brick Lane")))
    }

  }


}

class CLIFrontletSpec extends FunSpec with MustMatchers {

  import FrontletSpec._

  describe("A CLIFrontlet") {
    ignore("should work :)"){
      class Params extends CLIFrontlet[Params] {
        val file = StringSlot("file") cli("f",true)
      }
      val parsed = CLIParser.parse(Array("-f","test.csv"),new Params)
      val file = parsed.file()
      println(file)
    }
  }
}
