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

The `:=` operator is also aliased with the `apply(value)` method that can be chained:

```scala
val person = new Person().name("Riedel Castro").age(36)
```


All slot values are stored in, and retrieved from, an underlying mutable (or immutable, see below) scala map, in this case `person._map`.
This map can come from anywhere: json results of a web-service, bson query results of a mongo database etc. In these
cases frontlets give clients typed and concise access to the underlying raw map.

A good place for further information is the [spec][spec].

### Mongo Support ###

Frontlets particularly shine in combination with document-based nosql databases such as mongodb. You can
wrap frontlets around BSON objects retrieved from mongo databases. The frontlet library also provides
mongo collection wrappers that provide a powerful *typed* query interface close to the original raw
collection interface. This gives you succinct and typesafe queries while you can still fully
utilize the flexibility and power of mongo (without being at the mercy of magic object persistence frameworks).

A simple querying example:

```scala
val coll = mongoDB.getCollection("persons")
val persons = new MongoFrontletCollection(coll, () => new Person)
val old = persons.query(_.age.$gt(36),_.name.select)
val name = old.next().name()
```

The last query here returns all persons (frontlets) over 36, but only retrieves their name field.

Generic map-based data access can certainly be too slow for certain use cases, such as heavy computation in inner
loops. In such cases you want to convert frontlets into a more efficient representation. However, notice that data
is often read and then only used once or twice (say, to render on a webpage). In such cases frontlets
have very minimal overhead, as you would generally need to call the underlying, say, mongo map data structure
at least once anyway.

### JSON import and export ###

Frontlets can be imported and exported from JSON strings:

```scala
val person = new Person().setJSON("""{"age":36, "address":{"street":"Broadway","number":1}}""")
person.age() must be (36)
person.address().street() must be ("Broadway")
person.toJSON must be ("""{"age":36, "address":{"street":"Broadway","number":1}}""")
```

Currently frontlets use [jackson](http://jackson.codehaus.org/) and its
scala wrapper [jacks](https://github.com/wg/jacks) to implement this functionality.

### Persistent Object Graphs ###

Frontlets can also make reading complex object graphs from the "linear" mongo collections more convenient (and possibly
efficient), although this is highly **experimental**. For example, consider the following Frontlet:

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
it can be used with a graph loading routine that instantiates a graph of frontlets and their relations.
This graph, when declared as implicit, will then be used in the "dereference" * calls to the inverse
and reference slots:

```scala
//a mongo collection of nodes
val data = MongoFrontletCollection(coll, () => new Node)
val Seq(first,second) = data.take(2).toSeq

//find all nodes starting from the two given example nodes, and using the given neighborhood function
//this graph will be implicitly used in all ref slots and inverse slots when calling "*" methods
implicit val graph = GraphLoader.loadGraph(Seq(first,second), {
  case p: Node => Seq(p.children in data, p.parent in data)})

//the following calls actually use the above cache objects, and return the neighbor of the given object
//as stored in the graph.
println(first.parent.*)
println(first.children.*)
```

### Immutable Frontlets ###

You can use frontlets also in a immutable mode:

```scala
class ImmutablePerson extends OuterFrontlet[ImmutablePerson] {
  val age = IntSlot("age")
}
val person = new ImmutablePerson().age(18)
val changed = person.age := 36
person.age() must be (18)
changed.age() must be (36)
```

One of the perks of working with immutable frontlets is their support for manipulating nested immutable objects:

```scala
val person = new ImmutablePerson().age(18).address.create(_.street("Broadway"))
val changed = person.address(_.street("Brick Lane"))
changed must be (new ImmutablePerson().age(18).address.create(_.street("Brick Lane")))
```
Note that the second line creates new person "tree" in which the address "subtree" is replaced with a
new address containing a different street. For another way to deal with the problem of manipulating nested
immutable objects, see [zippers][zippers].


Installation
------------
You can use frontlets as an sbt dependency

    resolvers ++= Seq("UCLCL snapshots" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/snapshots",
                      "UCLCL releases" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/releases")

    libraryDependencies += "org.riedelcastro.frontlets" %% "frontlets" % "[VERSION]"

The library has been very useful already, but does not yet have a stable release. Feel free to use (at your own risk)
the most current release or snapshot.

Of course you can also download any version (click on "tags" to get a list of zip/tgz archives) or the current trunk,
and the build using `sbt`.

License
-------
Frontlets are licensed under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).

[spec]: https://github.com/riedelcastro/frontlets/blob/master/src/test/scala/org/riedelcastro/frontlets/FrontletSpec.scala
[zippers]: http://scienceblogs.com/goodmath/2010/01/13/zippers-making-functional-upda/
