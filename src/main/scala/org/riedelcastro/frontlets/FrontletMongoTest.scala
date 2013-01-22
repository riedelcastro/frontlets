package org.riedelcastro.frontlets

import com.mongodb.Mongo

/**
 * This class shows some example usage of frontlets and mongo serialization and querying.
 */
object FrontletMongoTest {

  import MongoFrontletImplicits._
  import MongoFrontletConverter._

  def main(args: Array[String]) {

    class Person extends Frontlet {
      val name = StringSlot("name")
      val age = IntSlot("age")
      val address = FrontletSlot("address", () => new Address)
      val hobbies = StringListSlot("hobbies")
      val spouse = RefSlot("spouse", () => new Person)
      val children = InverseSlot("children", (p: Person) => p.father)
      val father = RefSlot("father", () => new Person)
    }
    class Address extends Frontlet {
      val street = StringSlot("street")
      val zip = StringSlot("zip")
    }

    val address = new Address
    address.street := "Mass Ave."

    val james = new Person
    val laura = new Person
    val kid = new Person

    james.name := "James"
    james.Id := 1
    james.age := 50
    james.hobbies := Seq("photography", "soccer")
    james.address := address

    laura.name := "Laura"
    laura.Id := 2
    laura.age := 20
    laura.hobbies := Seq("James")
    laura.address := address

    kid.name := "Kid"
    kid.Id := 3

    //reference attributes
    james.spouse ::= laura
    laura.spouse ::= james
    kid.father ::= james

    //calling apply on a slot without parameters returns the value of the slot.
    println("apply method calls")
    println(james.age())
    //calling apply with a parameter sets the slot to the given value and returns the frontlet.
    println(kid.age(10))


    val mongoConn = new Mongo("localhost", 27017)
    val mongoDB = mongoConn.getDB("mongofrontlet-test")
    val coll = mongoDB.getCollection("persons")
    coll.drop()
    val persons = new MongoFrontletCollection(coll, () => new Person, (p: Person) => Seq(Seq(p.name))) with LazyFrontletConverter[Person]

    persons += james
    persons += laura
    persons += kid

    //find all persons with name = "James" and set their name slot to "Jamie"
    persons.update(_.name("James"), _.name.update("Jamie"))

    //iterate over all persons in the collection
    for (p <- persons) {
      println(p.asMap)
    }

    //find all people with age == 50 but only return their age and name slots.
    val queryResult = persons.query(_.age(50), _.age.select.name.select)
    //    val queryResult = persons.query(_.age.set(50))
    for (p <- queryResult) {
      println(p.asMap)
    }

    println("*** GT query:")
    println(persons.query(_.age.$gt(19),_.name.select).mkString("\n"))

    //test a delta update, laura turns 21 and is also interested in Rick!
    val updatedLaura = new Person
    updatedLaura.Id := 2
    updatedLaura.age := 21
    updatedLaura.hobbies := Seq("James", "Rick!")
    updatedLaura.address := address
    updatedLaura.spouse ::= james

    //todo: this seems to not keep the name (= attribute not set in updated frontlet)
    persons.updateDelta(laura, updatedLaura)

    println(persons.mkString("\n"))

    //test batch id query
    println("****")
    println(persons.query(_.hobbies.contains("James")).mkString("\n"))
    println(persons.query(_.Id.valuesIn(Seq(1, 2))).mkString("\n"))
    println(persons.query(_.Id(1)).mkString("\n"))

    //the graph loader loads a graph rooted around "james" by incrementally and recursively instantiating
    //the spouse of every person in the graph.
    implicit val refs = GraphLoader.load(Seq(james), {
      case p: Person => Seq(p.spouse in2 persons)
    })

    //the calls below use the implicit refs object returned by the graph loader to do dereferencing.
    println("James' spouse")
    println(james.spouse.deref)
    println("James' spouse's spouse")
    println(james.spouse.deref.spouse.deref)

    //same as above, but with both inverse and ref slots.
    implicit val graph = GraphLoader.loadGraph(Seq(kid), {
      case p: Person => Seq(p.children in persons, p.father in persons)
    })

    println("Index:")
    println(graph)
    println(james.children.value2(graph.index))
    println(james.children.*)
    println(kid.father.*)


    //ref slots need a Refs object (mapping from IDs to frontlets) and inv slots an inverter.
    println(james.children.value(GraphLoader.toInverter(graph.index)))
    println(kid.father.deref(graph.refs))

    println("Test Index 2")
    val index2 = GraphLoader.load2(Seq(james), {
      case p: Person => Seq(p.children in persons)
    })
    println(james.children.value(GraphLoader.toInverter(index2)))
    println(kid.father.deref(GraphLoader.toRefs(index2)))




    //or with fancy deref implicits
    //    import DerefImplicits._
    //    println(james.spouse-->spouse-->name.value)

    kid.name := "Kid 2"
    //more experimental stuff from here on:

    implicit val inverter = new CachedFunction(new LazyInverter(Map(manifest[Person] -> Seq(james, laura, kid))))

    println(james.children.value)

    val mongoInverter = new CachedFunction(new LazyMongoInverter(Map(manifest[Person] -> persons)))

    println(james.children.value(mongoInverter))

    val indexedInverter = new CachedFunction(new IndexedLazyInverter(Map(manifest[Person] -> Seq(james, laura, kid))))

    println(james.children.value(indexedInverter))

    //in memory caching
    implicit val indexer = new Indexer({
      case p: Person => Seq(p.name, p.age)
    })

    //these :=! calls inform the indexer of changes
//    james.age :=! 51
//    james.name :=! "Jamison"
//
//    println(indexer.index)

  }
}
