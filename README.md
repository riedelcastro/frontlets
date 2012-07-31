frontlets
=========

Frontlets are lightweight typed wrappers around scala maps, tailored for interaction with json and bson frameworks.

Usage
-----

A `Frontlet` is a simple scala class with `Slot` member objects that correspond to underlying map keys:

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

All slot values are stored in, and retrieved from, an underlying mutable scala map, in this case `person._map`.
This map can come from anywhere: json results of a web-service, bson query results of a mongo database etc. In these
cases frontlets give clients typed and concise access to the underlying raw map.

### Mongo Support ###

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

### Persistent Object Graphs ###

Frontlets can also make reading complex object graphs from the "linear" mongo collections more convenient (and possibly
efficient), although this is highly *experimental*. For example, consider the following Frontlet:

```scala
class Node extends Frontlet {
  val name = StringSlot("name")
  val parent = RefSlot("parent", () => new Node)
  val children = InverseSlot("children", (child:Node) => child.parent)
}
```

It uses a `RefSlot` which can point to any other node, and the corresponding `InverseSlot`, which
indicates that for each node there may be other nodes that have it as `parent`. The inverse children slot
does not store anything (by design it should not even cache any collection of children). However,
it can be used with a graph loading routine that generates cache objects which the reference and inverse slots
in turn can use as implicit parameters):

```scala
//a mongo collection of nodes
val graph = MongoFrontletCollection(coll, () => new Node)

//find all nodes starting from the two given example nodes, and using the given neighborhood function
val result = GraphLoader.load2(Seq(someNodeInTheGraph,anotherNode), {
  case N: Node => Seq(p.children of graph, p.parent of graph)})

//convert results to implicit cache objects that can be used in ref and inverse slots.
implicit val inverter = GraphLoader.toInverter(result)
implicit val index = GraphLoader.toRefs(result)

//the following calls actually use the above cache objects, and return the neighbor of the given object
//as stored in the graph and returned by the load2 method.
println(someNodeInTheGraph.parent.deref)
println(someNodeInTheGraph.children.value)
```

Installation
------------
You can use frontlets as a sbt dependency

    resolvers ++= Seq("IESL snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots",
                     "IESL releases" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases")

    libraryDependencies += "org.riedelcastro.frontlets" %% "frontlets" % "[VERSION]"

The library has been very useful already, but does not yet have a stable release. Feel free to use (at your own risk)
the most current release or snapshot.


