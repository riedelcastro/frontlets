frontlets
=========

Frontlets are lightweight typed wrappers around scala maps, tailored for interaction with json and bson frameworks.

Usage
-----

A `Frontlet` is a simple scala class with `Slot` objects that correspond to underlying map keys:

```scala
class Person extends Frontlet {
  val age = IntSlot("age")
  val name = StringSlot("name")
  val spouse = RefSlot("spouse", () => new Person)
}
```

Slots can be accessed and modified via Scala syntactic sugar:

```scala
val person = new Person
person.name := "Riedel Alejandro Castro Ruz"
person.age  := 36
println(person.name())
println(person.age())
```
Frontlets particularly shine in combination with document-based nosql databases such as mongodb. You can
wrap frontlets around BSON objects retrieved from mongo databases. The frontlet library alos provides
mongo collection wrappers that provide a powerful *typed* query interface close to the original raw
collection interface. This gives you succinct and typesafe queries while you can still fully
utilize the flexibility and power of mongo (without being at the mercy of magic object persistence frameworks).

A simple querying example:

```scala
val coll = mongoDB.getCollection("persons")
val persons = new MongoFrontletCollection(coll, () => new Person)
val old = persons.query(_.age.$gt(36),_.name.select)
val age = old.next().age()
```

The last query here returns all persons (frontlets) over 36, but only retrieves their name field.

Generic map-based data access can certainly be too slow for certain use cases, such as heavy computation in inner
loops. In such cases you want to convert frontlets into a more efficient representation. However, notice that data
is often read and then only used once or twice (say, to render on a webpage). In such cases frontlets
have very minimal overhead, as you would generally need to call the underlying, say, mongo map data structure
at least once.

Installation
------------
You can use frontlets as a sbt dependency

    resolvers ++= Seq("IESL snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots",
                     "IESL releases" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases")

    libraryDependencies += "org.riedelcastro.frontlets" %% "frontlets" % "0.1-SNAPSHOT"

The library has been very useful already, but does not yet have a stable release. Feel free to use (at your own risk)
the 0.1-SNAPSHOT in the meantime.


