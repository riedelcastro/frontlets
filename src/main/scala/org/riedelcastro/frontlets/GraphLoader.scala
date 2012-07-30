package org.riedelcastro.frontlets

import annotation.tailrec
import scala.collection.{MapProxy, Map => GenericMap, JavaConversions}
import collection.mutable.HashMap


object GraphLoader {

  case class SlotInCollection[+R <: Frontlet](slot: Frontlet#AbstractRefSlot[R], coll: AbstractFrontletCollection[R])

  type Refs = GenericMap[Any, Frontlet]
  type Invs = Frontlet#InverseSlot[Frontlet] => Iterable[Frontlet]

  //Map from (frontlet class, attribute name, value) to the frontlets of that class with that attribute value
  type Index = GenericMap[(Class[Frontlet], String, Any), Iterable[Frontlet]]

  /**
   * Loads a cache from ids to frontlets based on the root objects and a neighborhood function.
   * @param roots the frontlets to start with.
   * @param neighbors the neigborhood function from frontlets to (refslot,collection) pairs.
   * @param maxDepth How far from the root are we allowed to travel. If maxDepth == 0 no ids are added to the cache, for
   *                 maxDepth == 1 only the roots are added to the cache, for maxDeptn == 2 the roots immediate children etc.
   * @param oldRefs an existing cache. Frontlets with ids in this cache will not be loaded/traversed.
   * @return a cache that maps ids to the frontlet objects in the graph defined by the roots, neighborhood function and
   *         maximum depth.
   */
  @tailrec
  def load(roots: TraversableOnce[Frontlet],
           neighbors: PartialFunction[Frontlet, Seq[SlotInCollection[Frontlet]]],
           maxDepth: Int = Int.MaxValue,
           oldRefs: Refs = Map.empty): Refs = {

    if (maxDepth == 0) {
      oldRefs
    }
    else if (maxDepth == 1) {
      //fill-up roots into refs
      oldRefs ++ roots.map(c => c.id -> c).toMap
    }
    else {
      //fill-up roots into refs
      var refs = oldRefs ++ roots.map(c => c.id -> c).toMap

      //mapping from collections to the ids that need to be loaded
      val colls2ids = new HashMap[AbstractFrontletCollection[Frontlet], List[Any]]

      //gather ids to load for each collection
      for (c <- roots) {
        for (slots <- neighbors.lift(c)) {
          for (slot <- slots) {
            for (idRef <- slot.slot.opt) {
              if (!refs.isDefinedAt(idRef)) {
                colls2ids(slot.coll) = colls2ids.getOrElse(slot.coll, Nil) :+ idRef
              }
            }
          }
        }
      }

      //now do loading
      var loaded: List[Frontlet] = Nil
      for ((coll, ids) <- colls2ids) {
        loaded = loaded ++ coll.findByIds(ids).toList
      }

      //instantiate the yield
      if (loaded.size > 0) load(loaded, neighbors, maxDepth - 1, refs) else refs
    }

  }

  case class InvSlotInCollection[+R <: Frontlet](invSlot: Frontlet#AbstractInverseSlot[R], coll: AbstractFrontletCollection[R])


  //
  @tailrec
  def load2(roots: TraversableOnce[Frontlet],
            neighbors: PartialFunction[Frontlet, Seq[InvSlotInCollection[Frontlet]]],
            maxDepth: Int = Int.MaxValue,
            oldIndex: Index = Map.empty): Index = {

    if (maxDepth == 0) {
      oldIndex
    }
    else if (maxDepth == 1) {
      //fill-up roots into refs
      //todo: does this need to fill up inverse links too?
      oldIndex ++ roots.map(c => (c.frontletClass, c.Id.name, c.id) -> Seq(c))
      //oldGraph.copy(refs = oldGraph.refs ++ roots.map(c => c.id -> c).toMap)
    }
    else {
      //fill-up roots into refs
      var graph = oldIndex ++ roots.map(c => (c.frontletClass, c.Id.name, c.id) -> Seq(c))

      //mapping from collections and attributes to the values that need to be queried for.
      val collsAttr2ids = new HashMap[(InvSlotInCollection[Frontlet], String), List[Any]]

      //gather queries to execute
      for (c <- roots) {
        for (slots <- neighbors.lift(c)) {
          for (slotAndColl <- slots) {
            val invSlot = slotAndColl.invSlot
            val foreignSlot = invSlot.foreignSlot(slotAndColl.coll.prototype)
            val foreignFrontletClass = foreignSlot.frontlet.getClass.asInstanceOf[Class[Frontlet]]
            for (target <- invSlot.target) {
              val attrName = foreignSlot.name
              if (!graph.isDefinedAt((foreignFrontletClass, attrName, target))) {
                collsAttr2ids(slotAndColl -> attrName) = collsAttr2ids.getOrElse(slotAndColl -> attrName, Nil) :+ target
              }
            }
          }
        }
      }
      //now do the loading
      //todo: for every loaded frontlet we should also add the (frontlet.class,"_id", frontlet.id) -> frontlet mapping
      var loaded: List[Frontlet] = Nil
      for (((coll, attrName), targets) <- collsAttr2ids) {
        val prototype = coll.coll.prototype
        val foreignClass = prototype.frontletClass
        val result = coll.coll.findByAttribute(attrName, targets).toList
        //replace frontlets with already loaded frontlets with same id
        val deduped = result.map(c => {
          val existing = graph.get((foreignClass, prototype.Id.name, c.id))
          existing match {
            case Some(refs) =>
              refs.head
            case _ =>
              loaded = loaded :+ c
              graph = graph + ((foreignClass, prototype.Id.name, c.id) -> List(c))
              c
          }
        })
        val grouped = deduped.groupBy(c => {
          val foreignSlot = coll.invSlot.foreignSlot(c)
          val foreignValue = foreignSlot.value
          (foreignClass, attrName, foreignValue)
        })
        graph = graph ++ grouped
      }
      if (loaded.size > 0) load2(loaded, neighbors, maxDepth - 1, graph) else graph
    }

  }

  def toInverter(index: Index) = new IndexBasedInverter(index)

  def toRefs(index: Index) = index.filter(_._1._2 == "_id").map(pair => pair._1._3 -> pair._2.head)

  class IndexBasedInverter(index: Index) extends (Frontlet#InverseSlot[Frontlet] => Iterable[Frontlet]) {
    val prototypeCache = new HashMap[Manifest[Frontlet], Frontlet]

    def apply(v1: Frontlet#InverseSlot[Frontlet]) = {
      val prototype = prototypeCache.getOrElseUpdate(v1.manifest, v1.manifest.erasure.newInstance().asInstanceOf[Frontlet])
      val prototypeClass = prototype.frontletClass
      val attrName = v1.slot(prototype).name
      index((prototypeClass, attrName, v1.target.get))
    }
  }


}
