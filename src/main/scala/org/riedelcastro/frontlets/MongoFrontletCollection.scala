package org.riedelcastro.frontlets

import org.bson.BSONObject
import scala.collection.JavaConversions._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import org.bson.types.BasicBSONList
import scala.annotation.tailrec
import collection.{Map => GenericMap, mutable, MapProxy, JavaConversions}
import scala.collection.mutable.{HashSet, ArrayBuffer, HashMap, Map => MutableMap}
import scala._
import scala.Predef._


/**
 * Covariant interface to a frontlet collection.
 *
 * @tparam C Frontlet type of collection.
 */
trait AbstractFrontletCollection[+C <: AbstractFrontlet] extends Iterable[C] {

  /**
   * Find all frontlets for which the id is within the given set of ids.
   * @param ids the ids to check against.
   * @return all frontlets in this collection which have one of the provided ids.
   */
  def findByIds(ids: Seq[Any]): Iterator[C]

  /**
   * Find all frontlets for which the attribute/slot with the given name has a value
   * within the provided sequence of values.
   * @param name the name of the attribute.
   * @param values the sequence of values the attribute can be in.
   * @tparam T the type of the values the attribute can have.
   * @return all frontlets that have a slot with the given name for which the value is in the
   *         specified sequence of values.
   */
  def findByAttribute[T](name: String, values: Seq[T]): Iterator[C]

  //  def findByAttribute[T](slot:C=>C#Slot, values:Seq[T]):Iterator[C]

  /**
   * Find all frontlets for which the provided field/slot has one of the provided values.
   * @param field a function from frontlet to the slot to test.
   * @param values the values the slot can have to be included in the result.
   * @tparam T the type of the slot values.
   * @return all frontlets in this collection for which the given slot has one of the given
   *         values.
   */
  def findBySlot[T](field: C => AbstractFrontlet#Slot[T], values: Seq[T]): Iterator[C]

  /**
   * A prototype frontlet of the type this collection contains.
   * @return a prototype frontlet of the same type this collection holds.
   */
  def prototype: C
}

/**
 * A frontlet collection that can be modified (hence not covariant anymore).
 * @tparam C Frontlet type of collection.
 */
trait MutableFrontletCollection[C <: AbstractFrontlet] extends AbstractFrontletCollection[C] {
  /**
   * Assuming that the collection contains the old-frontlet, this operation changes
   * the old-frontlet to be in the state of the new frontlet. This assumes that
   * both frontlets have the same id.
   * @param oldFrontlet the old state of the frontlet.
   * @param newFrontlet the new state of the frontlet.
   */
  def updateDelta(oldFrontlet: C, newFrontlet: C)

  /**
   * Removes all frontlets that match the given query.
   * @param query a function that takes a frontlet and returns a query frontlet.
   */
  def remove(query: C => C)

  /**
   * Inserts the given frontlet into the collection.
   * @param c the frontlet to add.
   */
  def +=(c: C)

  /**
   * Batch insert of several frontlets. Can often be more efficient because underlying
   * persistence layers can avoid network overhead.
   * @param c the frontlets to add.
   */
  def ++=(c: TraversableOnce[C])

  /**
   * Deletes the complete collection.
   */
  def drop()
}

/**
 * A MongoFrontletCollection stores frontlets in a Mongo collection.
 *
 * @param coll the mongo collection that will be used to store the frontlets.
 * @param constructor the constructor to use when creating frontlets based on mongo objects.
 * @param indices A sequence of sequences of slots. Each slot sequence represents one
 *                multi-field index.
 * @tparam C the type of the frontlets this collection stores.
 * @author sriedel
 */
class MongoFrontletCollection[C <: AbstractFrontlet](val coll: DBCollection,
                                                     val constructor: () => C,
                                                     val indices: C => Seq[Seq[C#AbstractSlot[Any]]] = (c: C) => Seq.empty[Seq[C#AbstractSlot[Any]]])
  extends MutableFrontletCollection[C] with MongoFrontletConverter[C] {

  import MongoFrontletConverter._

  /**
   * Makes sure that the underlying collection has the specified indices.
   */
  private def ensureIndices() {
    val c = constructor()
    val indices = this.indices(c)
    for (index <- indices) {
      val dbo = new BasicDBObject()
      for (key <- index) dbo.put(key.name, 1)
      coll.ensureIndex(dbo)
    }
  }

  ensureIndices()

  /**
   * Returns a iterator over the frontlets stored in this collection.
   * @return frontlet iterator.
   */
  def iterator: CursorIterator = {
    new CursorIterator(coll.find())
  }

  /**
   * The size of the collection.
   * @return number of frontlets in the collection.
   */
  override def size = coll.count.toInt

  /**
   * Drops the underlying database collection.
   */
  def drop() {
    coll.drop()
  }

  lazy val prototype = constructor()

  def findByAttribute[T](name: String, values: Seq[T]): CursorIterator = {
    new CursorIterator(coll.find(new BasicDBObject(name, new BasicDBObject("$in", toMongo(values)))))
  }

  //todo: should override other default implementations---how much is this like the casbah MongoCollection?

  /**
   * Inserts a frontlet into the collection.
   * @param c the frontlet to add.
   */
  def +=(c: C) {
    coll.insert(eagerDBO(c))
  }

  /**
   * Batch insert of a collection of frontlets.
   * @param c collection to insert.
   */
  def ++=(c: TraversableOnce[C]) {
    coll.insert(JavaConversions.seqAsJavaList(c.map(eagerDBO(_)).toSeq))
  }

  case class Modification(op: String, value: Any)

  /**
   * Returns the modification operation and value needed to store the change represented by the old and new value.
   * @param oldOpt either None (if no value existed) or Some(x) where x is the old value
   * @param newOpt either None (if no new value exists) or Some(x) where x is the new value
   * @return A modification object that shows which mongo modifier is required, and what its value should be.
   */
  private def modification(oldOpt: Option[Any], newOpt: Option[Any]): Modification = {
    oldOpt -> newOpt match {
      case (Some(s1: Seq[_]), Some(s2: Seq[_])) if (s2.startsWith(s1)) =>
        Modification("$pushAll", toMongo(s2.drop(s1.size)))
      case (Some(oldValue), Some(newValue)) =>
        Modification("$set", toMongo(newValue))
      case (Some(oldValue), None) =>
        Modification("$unset", 1)
      case (None, Some(newValue)) =>
        Modification("$set", toMongo(newValue))
      case _ => sys.error("One argument should be Some(x)")
    }
  }

  /**
   * Efficiently stores the changes made to a given frontlet.
   * @param oldFrontlet an old version of the frontlet to store
   * @param newFrontlet a new version of the frontlet to store
   * @return a mongodb WriteResult.
   */
  def updateDelta(oldFrontlet: C, newFrontlet: C) = {
    require(oldFrontlet.id == newFrontlet.id)
    val keys = oldFrontlet.asMap.keySet ++ newFrontlet.asMap.keySet
    val insertDBO = new BasicDBObject()
    for (key <- keys; if (key != "_id")) {
      val mod = modification(oldFrontlet.get(key), newFrontlet.get(key))
      val bag = insertDBO.getOrElseUpdate(mod.op, new BasicDBObject()).asInstanceOf[DBObject]
      bag.put(key, mod.value)
    }
    val queryDBO = new BasicDBObject("_id", oldFrontlet.id)
    coll.update(queryDBO, insertDBO)
  }

  private def safeDbo(f: C => C) = if (f == null) null else eagerDBO(f(constructor()))

  /**
   * Finds all frontlets that match the given queries and instantiates the selected slots of these frontlets.
   * @param query a function that maps a frontlet to a frontlet that should be matched.
   * @param select a function that maps a frontlet to a frontlet that shows which slots to instantiate
   *               (using the select method). If null all slots are selected
   * @return an iterator over frontlets as defined above.
   */
  def query(query: C => C = null.asInstanceOf[C => C], select: C => C = null.asInstanceOf[C => C]) = {
    val queryDBO = safeDbo(query)
    val selectDBO = safeDbo(select)
    new CursorIterator(coll.find(queryDBO, selectDBO))
  }

  def findByIds(ids: Seq[Any]) = {
    query((c:C) => new MongoSlot(c.Id).valuesIn(ids).asInstanceOf[C])
  }

  def findBySlot[T](field: (C) => AbstractFrontlet#Slot[T], values: Seq[T]) = {
    query(c => new MongoSlot[AbstractFrontlet, T](field(c)).valuesIn(values).asInstanceOf[C])
  }


  /**
   * Removes all items in the collection that match the given query.
   * @param query a function that maps a frontlet to a frontlet that should be matched.
   * @return unit
   */
  def remove(query: C => C) = {
    val queryDBO = safeDbo(query)
    coll.remove(queryDBO)
  }

  /**
   * Updates the collection as specified.
   * @param query a function that returns a frontlet to match
   * @param modification a function that returns the modification frontlet.
   * @param upsert should an object be created if no match can be found.
   * @param multi should we do updates to all matching frontlets.
   * @return a mongo write result.
   */
  def update(query: C => C = null.asInstanceOf[C => C],
             modification: C => C = null.asInstanceOf[C => C],
             upsert: Boolean = false,
             multi: Boolean = false) = {
    val queryDBO = safeDbo(query)
    val modDBO = safeDbo(modification)
    coll.update(queryDBO, modDBO, upsert, multi)
  }

  /**
   * By default the collection converts mongo documents eagerly to scala map
   * objects which then become the maps underlying the frontlets.
   * @param dbo a mongo document object.
   * @param constructor the constructor to use to create the frontlets.
   * @return a frontlet based on the provided dbo.
   */
  def mongo2Frontlet(dbo: DBObject, constructor: () => C) = {
    MongoFrontletConverter.eagerFrontlet(dbo, constructor)
  }

  /**
   * A wrapper around a mongodb database iterator. On each
   * call of next it converts the given document to a frontlet.
   * @param underlying the underyling raw database iterator.
   */
  class CursorIterator(val underlying: DBCursor) extends Iterator[C] {
    def skip(amount: Int) = new CursorIterator(underlying.skip(amount))

    def limit(amount: Int) = new CursorIterator(underlying.limit(amount))

    def next() = mongo2Frontlet(underlying.next(), constructor)

    def hasNext = underlying.hasNext

    def headOption = if (hasNext) Some(next()) else None

    def setNoTimeOut() {
      underlying.setOptions(16)
    }

    def sort(fields: C => C) = {
      val fieldsDBo = safeDbo(fields)
      new CursorIterator(underlying.sort(fieldsDBo))
    }

    def close() {
      underlying.close()
    }
  }

}

/**
 * A trait to mix-in into mongo frontlet collections that controls how the raw
 * mongo documents are converted to frontlets.
 * @tparam C the type of frontlets to convert to.
 */
trait MongoFrontletConverter[C <: AbstractFrontlet] {
  /**
   * Converts a mongodb document to a frontlet
   * @param dbo the mongodb doc.
   * @param constructor the constructor to use for creating the frontlet.
   * @return a frontlet based on the content of the given mongo document.
   */
  def mongo2Frontlet(dbo: DBObject, constructor: () => C): C
}

/**
 * A converter that eagerly, and recursively, creates a mutable.Map based on the
 * given mongodb document. This eager conversion translates DBObjects to mutable.Maps,
 * and lists to scala Seq objects. All other types are kept as is.
 * @tparam C the type of frontlets to convert to.
 */
trait EagerFrontletConverter[C <: Frontlet] extends MongoFrontletConverter[C] {
  abstract override def mongo2Frontlet(dbo: DBObject, constructor: () => C) = {
    MongoFrontletConverter.eagerFrontlet(dbo, constructor)
  }
}

/**
 * This converter creates frontlets which do conversion from mongodb objects to
 * scala maps and seqs on-the-fly. That is, no conversion is performed at creation
 * time (or call time of mongo2Frontlet). Instead, the underyling map of the frontlet
 * keeps the the original mongodb object around, and when its get/set methods are called
 * the map converts the mongodb fields into corresponding scala values.
 *
 * @tparam C the type of frontlet the converter creates.
 */
trait LazyFrontletConverter[C <: Frontlet] extends MongoFrontletConverter[C] {
  abstract override def mongo2Frontlet(dbo: DBObject, constructor: () => C) = {
    MongoFrontletConverter.lazyFrontlet(dbo, constructor)
  }
}


/**
 * Helper methods to convert frontlets into mongo objects and vice versa.
 */
object MongoFrontletConverter {

  def eagerDBO(frontlet: AbstractFrontlet): DBObject = {
    toMongo(frontlet.asMap).asInstanceOf[DBObject]
  }

  def eagerFrontlet[C <: AbstractFrontlet](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c.setMap(toFrontlet(dbo).asInstanceOf[mutable.Map[String, Any]])
    c
  }

  def lazyFrontlet[C <: AbstractFrontlet](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c.setMap(toLazyFrontlet(dbo).asInstanceOf[mutable.Map[String, Any]])
    c
  }


  def toMongo(any: Any): Any = {
    any match {
      case smap: scala.collection.Map[_, _] =>
        val dbMap = new BasicDBObject(smap.size)
        for ((key, value) <- smap) {
          val mongoValue = toMongo(value)
          dbMap.put(key.asInstanceOf[String], mongoValue)
        }
        dbMap
      case l: Seq[_] =>
        val dbList = new BasicDBList
        for (element <- l) dbList.add(toMongo(element).asInstanceOf[AnyRef])
        dbList
      case _ => any
    }
  }

  def toFrontlet(any: Any): Any = {
    any match {
      case dbList: BasicBSONList =>
        val list = new ArrayBuffer[Any]
        for (element <- dbList) list += toFrontlet(element)
        list
      case dbo: DBObject =>
        val map = new mutable.HashMap[String, Any]
        for (key <- dbo.keySet()) map(key) = toFrontlet(dbo.get(key))
        map
      case _ => any
    }
  }

  def toLazyFrontlet(any: Any): Any = {
    any match {
      case dbList: BasicBSONList => new BasicBSONBSeq(dbList)
      case dbo: BSONObject => new BSONMap(dbo)
      case _ => any
    }
  }


}

/**
 * A wrapper around a frontlet that can be used to specify mongo queries using the frontlet map.
 * @param frontlet the frontlet to wrap around
 * @tparam C the type of frontlet.
 */
class MongoFrontlet[C <: AbstractFrontlet](val frontlet: C) {


}

class MongoIntSlot[C <: AbstractFrontlet](val slot: C#IntSlot) {

  def comparator(comp: String, that: Int): C#FrontletType = {
    slot.frontlet.assign(slot.name, Map(comp -> that))
  }

  def $gt(that: Int) = comparator("$gt", that)

  def $lt(that: Int) = comparator("$lt", that)

  def $gte(that: Int) = comparator("$gte", that)

  def $lte(that: Int) = comparator("$lte", that)


}

/**
 * A slot that can do mongo specific operations
 * @param slot the original slot in the frontlet
 * @tparam C the frontlet type.
 * @tparam V the value type of the slot.
 */
class MongoSlot[C <: AbstractFrontlet, V](val slot: C#BasicSlot[V]) {

  /**
   * Changes the query frontlet to select/project the given slot.
   * @return the frontlet container of this slot.
   */
  def select: C = {
    slot.rawPut(1)
    slot.frontlet
  }


  /**
   * Creates a query frontlet that will update the given slot for matched frontlets
   * to have the given values.
   * @param value the new slot value for matching frontlets to be set to.
   * @return the encompassing query frontlet.
   */
  def update(value: V): C#FrontletType = {
    val map = slot.frontlet.get("$set").getOrElse(Map.empty).asInstanceOf[Map[String, Any]]
    slot.frontlet.assign("$set", map + (slot.name -> value))
  }

  /**
   * Modifies the query to match if the given slot has values in the given
   * set of values.
   * @param values the values to match against.
   * @return the encompassing query frontlet.
   */
  def valuesIn(values: Seq[V]): C#FrontletType = {
    slot.rawPut(Map("$in" -> values))
  }

  /**
   * Modifies the query to test whether the frontlet has the given field or not.
   * @param yes if true the query requires the frontlet to have the given slot, else it
   *            requires the frontlet to not have the given slot.
   * @return the encompassing query frontlet.
   */
  def exists(yes: Boolean): C#FrontletType = {
    slot.rawPut(Map("$exists" -> yes))
  }

}


class MongoRefSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](val slot: C#AbstractRefSlot[A]) {
  def in(coll: MongoFrontletCollection[A]): GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(slot, coll)

}

/**
 * Support for mongo queries specific to list attributes
 * @param slot a list slot
 * @tparam C the type of frontlet the attribute is part of
 * @tparam A the type objects in the list.
 */
class MongoPrimitiveListSlot[C <: AbstractFrontlet, A](val slot: C#PrimitiveListSlot[A]) {
  /**
   * Returns a frontlet that mongo can use to match documents that have the given value as member in the list.
   * @param a the element that needs to be in the list.
   * @return the frontlet itself.
   */
  def contains(a: A): C = {
    slot.rawPut(a)
    slot.frontlet
  }

}


class MongoInvSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](val slot: C#AbstractInverseSlot[A]) {
  def of(coll: MongoFrontletCollection[A]): GraphLoader.InvSlotInCollection[A] =
    GraphLoader.InvSlotInCollection(slot, coll)
}


/**
 * Lazy scala seq wrapper around bson sequence.
 * @param bson the bson list to wrap around.
 */
class BasicBSONBSeq(val bson: BasicBSONList) extends Seq[Any] {

  import MongoFrontletConverter._

  def length = bson.size()

  def apply(idx: Int) = toLazyFrontlet(bson.get(idx))

  def iterator = bson.iterator().map(toLazyFrontlet(_))
}

/**
 * Wrapper around a bson map to provide scala-access to the map and its content. It recursively
 * converts values in the map into their scala equivalent (in case they are bson-based).
 * @param bson the underlying BSON map.
 */
class BSONMap(val bson: BSONObject) extends collection.mutable.Map[String, Any] {

  import MongoFrontletConverter._

  def get(key: String) = if (bson.containsField(key)) Some(toLazyFrontlet(bson.get(key))) else None

  def iterator = bson.keySet().iterator().map(key => key -> toLazyFrontlet(bson.get(key)))

  def +=(kv: (String, Any)): this.type = {
    bson.put(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = {
    bson.removeField(key)
    this
  }
}

/**
 * Implicits to import when you want to create frontlet mongo queries.
 */
object MongoFrontletImplicits {

  implicit def toMongoSlot[C <: AbstractFrontlet, V](slot: C#BasicSlot[V]) = new MongoSlot(slot)

  implicit def toMongoIntSlot[C <: AbstractFrontlet, V](slot: C#IntSlot) = new MongoIntSlot(slot)

  implicit def toMongoRefSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot)

  implicit def toMongoInvSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](slot: C#AbstractInverseSlot[A]) = new MongoInvSlot(slot)

  implicit def toMongoPrimitiveListSlot[C <: AbstractFrontlet, A](slot: C#PrimitiveListSlot[A]) = new MongoPrimitiveListSlot(slot)

  implicit def toMongoFrontlet[C <: AbstractFrontlet](frontlet: C) = new MongoFrontlet(frontlet)

}

object DerefImplicits {
  implicit def toMongoRefSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot) {
    def -->(implicit cache: GenericMap[Any, Frontlet]): A = slot.deref(cache)
  }

  implicit def toMongoInvSlot[C <: AbstractFrontlet, A <: AbstractFrontlet](slot: C#InverseSlot[A]) = new MongoInvSlot(slot) {
    //    def -->(implicit cache: GenericMap[Any, Frontlet]): A = slot.deref(cache)
  }

}


class CachedFunction[F, T](val delegate: F => T) extends Map[F, T] {
  val cache = new HashMap[F, T]

  def get(key: F) = Some(cache.getOrElseUpdate(key, delegate(key)))

  def iterator = cache.iterator

  def -(key: F) = {
    val result = new CachedFunction(delegate)
    result.cache ++= cache
    result.cache -= key
    result
  }

  def +[B1 >: T](kv: (F, B1)) = {
    val result = new CachedFunction(delegate)
    result.cache ++= cache
    result.cache += kv.asInstanceOf[(F, T)]
    result
  }
}

class LazyInverter(val frontlets: PartialFunction[Manifest[AbstractFrontlet], Iterable[AbstractFrontlet]])
  extends (AbstractFrontlet#InverseSlot[AbstractFrontlet] => Iterable[AbstractFrontlet]) {
  def apply(slot: AbstractFrontlet#InverseSlot[AbstractFrontlet]) = {
    val typed = slot.asInstanceOf[AbstractFrontlet#InverseSlot[AbstractFrontlet]]
    val result = frontlets.lift(slot.manifest).getOrElse(Nil).filter(c => typed.slot(c).opt == Some(typed.frontlet.id))
    result
  }
}

class IndexedLazyInverter(val frontlets: PartialFunction[Manifest[AbstractFrontlet], Iterable[AbstractFrontlet]])
  extends (AbstractFrontlet#InverseSlot[AbstractFrontlet] => Iterable[AbstractFrontlet]) {


  val index = new mutable.HashMap[(AbstractFrontlet#AbstractRefSlot[AbstractFrontlet], Any), Seq[AbstractFrontlet]]
  val indexed = new mutable.HashSet[AbstractFrontlet#AbstractRefSlot[AbstractFrontlet]]
  val prototypes = new mutable.HashMap[Manifest[AbstractFrontlet], Option[AbstractFrontlet]] //frontlets.map(p => p._1 -> p._2.headOption)

  def findFrontletsWhereRefSlotIs(refSlotFunction: AbstractFrontlet => AbstractFrontlet#AbstractRefSlot[AbstractFrontlet],
                                  id: Any,
                                  inWhere: Iterable[AbstractFrontlet],
                                  ofType: Manifest[AbstractFrontlet]) = {
    {
      for (prototype <- prototypes.getOrElseUpdate(ofType, frontlets(ofType).headOption);
           refSlot = refSlotFunction(prototype)) yield {
        if (!indexed(refSlot)) {
          for (c <- frontlets.lift(ofType).getOrElse(Nil); if (refSlotFunction(c).opt == Some(id))) {
            index(refSlot -> id) = index.getOrElse(refSlot -> id, Nil) :+ c
          }
          indexed += refSlot
        }
        index.getOrElse(refSlot -> id, Nil)
      }
    }.getOrElse(Nil)
  }

  def apply(slot: AbstractFrontlet#InverseSlot[AbstractFrontlet]) = {
    val typed = slot.asInstanceOf[AbstractFrontlet#InverseSlot[AbstractFrontlet]]
    findFrontletsWhereRefSlotIs(typed.slot, typed.frontlet.id, frontlets.lift(typed.manifest).getOrElse(Nil), typed.manifest)
  }
}


class LazyMongoInverter(val frontlets: PartialFunction[Manifest[AbstractFrontlet], AbstractFrontletCollection[AbstractFrontlet]],
                        val cache: GenericMap[Any, AbstractFrontlet] = Map.empty)
  extends (AbstractFrontlet#InverseSlot[AbstractFrontlet] => Iterable[AbstractFrontlet]) {
  def apply(slot: AbstractFrontlet#InverseSlot[AbstractFrontlet]) = {
    val typed = slot.asInstanceOf[AbstractFrontlet#InverseSlot[AbstractFrontlet]]
    val found = for (coll <- frontlets.lift(slot.manifest)) yield {
      val raw = coll.findBySlot(c => slot.slot(c).asInstanceOf[AbstractFrontlet#RefSlot[AbstractFrontlet]], Seq(typed.frontlet.id))
      raw.map(c => cache.getOrElse(c.id, c)).toSeq
    }
    found.getOrElse(Nil)
  }
}

class Indexer(val indices: Frontlet => Seq[Frontlet#AbstractSlot[Any]]) extends Function2[Frontlet#AbstractSlot[Any], Any, Unit] {

  case class SlotKey(frontletClass: Class[Frontlet], name: String, value: Any) {
  }

  def slotKey(slot: Frontlet#AbstractSlot[Any], value: Any) = {
    SlotKey(slot.frontlet.getClass.asInstanceOf[Class[Frontlet]], slot.name, value)
  }

  val index = new HashMap[SlotKey, List[Frontlet]]

  //val index = new HashMap[]
  def apply(slot: Frontlet#AbstractSlot[Any], value: Any) {
    //todo: odd cast
    val typed = slot.frontlet.asInstanceOf[Frontlet]
    if (indices(typed).contains(slot)) {
      val key = slotKey(slot, value)
      index(key) = index.getOrElse(key, Nil) :+ typed
    }
  }
}


