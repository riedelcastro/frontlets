/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package org.riedelcastro.frontlets

import collection.mutable
import util.parsing.json.JSON

/**
 * A Frontlet provides typed access to an underlying Map data-structure. This map can come
 * from various sources, such as JSON objects or MongoDB objects.
 *
 * Frontlets are designed
 * to make access to (often persistent) data safe, easy and generic. This comes at
 * the price of efficiency, so clients should not access Frontlets in inner-loops
 * of computations. In practice this may require a further conversion step from Frontlets
 * into a more efficient representation.
 */
class Frontlet {
  thisFrontlet =>


  //  def this(map:scala.collection.mutable.HashMap[String,Any]) = { this(); this._map = map }
  // Managing raw underlying map that hold the data

  /**
   * The type of underlying Map. We use the generic scala mutable Map interface.
   * If clients want to use their own Map data structure (such as MongoDB maps) they
   * only need to provide a wrapper mutable.Map implementation around their data.
   */
  type MapType = mutable.Map[String, Any]

  /**
   * Set the underlying map for this frontlet.
   * @param map the map that will be used as underlying datastructure
   * @return this frontlet.
   */
  def setMap(map: MapType): this.type = {
    _map = map
    this
  }

  /**
   * Constructor that takes the underlying map.
   * @param map the underlying map to use.
   */
  def this(map: mutable.Map[String, Any]) = {
    this()
    this._map = map
  }

  /**
   * Underlying map member.
   * todo: fix __map vs _map confusion one day.
   */
  private var __map: MapType = null


  /**
   * Returns underlying map.
   * todo: remove this
   * @return the current underlying map. If empty, a new map is created and set.
   */
  def _map = {
    if (__map == null) __map = new mutable.HashMap[String, Any]
    __map
  }

  /**
   * Alternative method to set underyling map.
   * todo: remove this.
   * @param map the underlying map.
   */
  def _map_=(map: mutable.Map[String, Any]) {
    __map = map
  }

  /**
   * Creates a default map.
   * @return a default map to be used as underyling map.
   */
  def _newDefaultMap: MapType = new scala.collection.mutable.HashMap[String, Any]

  @deprecated
  def _rawGet(name: String): Any = {
    _map(name)
  }

  /**
   * Prints out the underlying map
   * @return a string representation of the underyling map.
   */
  override def toString = _map.toString()

  /**
   * The map key to use for the ID field.
   * @return the key to use for the id field.
   */
  def idName = "_id"

  /**
   * Typed access to the frontlet class.
   * @return the Frontlet class.
   */
  def frontletClass = getClass.asInstanceOf[Class[Frontlet]]

  /**
   * Create a new random ID.
   * @return random ID.
   */
  def newId = java.util.UUID.randomUUID.timestamp

  /**
   * Returns the ID of the frontlet. Creates a new ID if no ID has yet been set.
   * @return an identifier for this frontlet.
   */
  final def id: Any = {
    // "final" because we need to ensure that the _id gets inserted into the
    var result = _rawGet(idName) // avoid getOrElseUpdate because it will allocate a closure object
    if (result != null) result
    else {
      result = newId
      _map.update(idName, result)
      result
    }
  }

  //todo: maps throw exceptions when key is not defined, need to adapt requirement
  /**
   * Set an id for this frontlet.
   * @param i an ID. Generally can be any type of object, but during serialization
   *          some types may not be storable by the underyling serialization framework.
   */
  def id_=(i: Any) {
    _map.update(idName, i)
  }

  // Classes for holding key:value pairs

  /**
   * A Frontlet has a collection of slots (or fields) that store the attributes of the
   * frontlet. These slots map one-to-one to the fields of the underlying map.
   * @tparam T the type of the attribute.
   */
  trait AbstractSlot[+T] {

    /**
     * The current value of the slot.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: T

    /**
     * Convenience method for more concise access to slot value.
     * @return value of the slot.
     */
    def apply() = value

    /**
     * The name of this slot. This corresponds to the map field the slot is using
     * to store its value.
     * @return
     */
    def name: String

    /**
     * Returns Some(value) if the frontlet has the given slot, or None otherwise.
     * @return Some(value) if the frontlet has the given slot, or None otherwise.
     */
    def opt: Option[T]

    /**
     * The frontlet this slot is contained in.
     * @return this frontlet.
     */
    def frontlet: thisFrontlet.type = thisFrontlet

  }

  /**
   * Every frontlet has an ID. This ID is itself a field of the underlying map, and hence
   * can also be accessed through a slot.
   */
  object Id extends AbstractSlot[Any] {
    def name = "_id"

    def value = id

    def opt = Some(id)

    def :=(newId: Any) {
      frontlet.id = newId
    }

    def set(newId: Any): thisFrontlet.type = {
      this := newId
      thisFrontlet
    }

    def apply(newId: Any): thisFrontlet.type = {
      set(newId)
    }
  }

  //  val idSlot = new IdSlot

  /**
   * An AbstractInverseSlot is a slot that contains a collection of
   * other frontlets, namely the ones that have the given target value as
   * value of the given foreign slot. This type of slot is fundamentally
   * different to the standard slot, which stores its values in the underlying map.
   *
   * @tparam A the type of frontlets this slot contains.
   */
  sealed trait AbstractInverseSlot[+A <: Frontlet] {
    /**
     * The name of this slot.
     * @return a string name.
     */
    def name: String

    /**
     * The slot of the frontlets that needs to have a particular target value
     * for the frontlet to be in this slot.
     * @return the foreign slot.
     */
    def foreignSlot: Frontlet => A#AbstractSlot[Any]

    /**
     * Can there be several frontlets with the target value?
     * @return true iff there can be several frontlets with the given target value.
     */
    def unique: Boolean = false

    /**
     * What value does the foreign slot should have.
     * @return Some(target-value) or None if this frontlet has no values in this slot.
     */
    def target: Option[Any]
  }

  /**
   * The Inverse slot is a default implementation of the AbstractInverseSlot.
   * @param name the name for this slot.
   * @param slot the foreign slot.
   * @param m a manifest.
   * @tparam A the type of frontlets this slot contains.
   */
  case class InverseSlot[A <: Frontlet](name: String,
                                        slot: A => A#AbstractRefSlot[Frontlet])(implicit m: Manifest[A])
    extends AbstractInverseSlot[A] {

    /**
     * This method returns some set of frontlets associated with this slot based
     * on the provided cache object.
     * @param cache A mapping from frontlet slots to iterables of frontlets. The iterable
     *              for this  frontlet slot should correspond to a set of frontlets that
     *              have the given target value for the given slot.
     * @return all frontlets that are associated with this slot in the cache.
     */
    def value(implicit cache: Frontlet#InverseSlot[Frontlet] => Iterable[Frontlet]): Iterable[A] = {
      cache(this.asInstanceOf[InverseSlot[Frontlet]]).asInstanceOf[Iterable[A]]
    }

    //todo: this is probably very slow, as I need access the manifest, erasure, create new object etc.
    def value2(implicit cache: collection.Map[(Class[Frontlet], String, Any), Iterable[Frontlet]]) = {
      val foreignFrontlet = m.erasure.newInstance().asInstanceOf[A]
      val foreignSlot = slot(foreignFrontlet)
      cache((foreignFrontlet.frontletClass, foreignSlot.name, frontlet.id)).asInstanceOf[Iterable[A]]
    }

    def foreignSlot = (c: Frontlet) => slot(c.asInstanceOf[A])

    def target = Some(frontlet.id)

    def manifest = m.asInstanceOf[Manifest[Frontlet]]

    def frontlet: thisFrontlet.type = thisFrontlet

  }

  /**
   * Default implementation of an AbstractSlot.
   * @param name the name of the slot.
   * @tparam T the type of the attribute.
   */
  abstract class Slot[T](val name: String) extends AbstractSlot[T] {
    def value: T

    /**
     * Set the value for this slot.
     * @param value value to set.
     */
    def :=(value: T)

    /**
     * Set a value for this slot but inform the provided hook before this happens.
     * @param value the value to set.
     * @param preHook the hook to call before setting the value.
     */
    def :=!(value: T)(implicit preHook: Function2[Frontlet#AbstractSlot[Any], Any, Unit]) {
      preHook(this, value)
      this := value
    }

    /**
     * Set the value and return the containing Frontlet.
     * @param value the value to set.
     * @return the frontlet this slot belongs to.
     */
    def apply(value: T): thisFrontlet.type = set(value)


    def opt = if (_map.isDefinedAt(name)) Some(value) else None

    /**
     * Set a raw value into the underlying map. Should generally only be used
     * in other library code.
     * @param value the value to be set.
     */
    def rawPut(value: Any) {
      _map.update(name, value)
    }

    /**
     * Does the frontlet have this slot.
     * @return true iff the underyling map has this slot.
     */
    def isDefined: Boolean = _map.isDefinedAt(name)

    /**
     * Set the value of this slot using an option. If parameter
     * is Some(value) the value is set, if None nothing is changed.
     * @param opt the option to use.
     */
    def :=(opt: Option[T]) {
      for (value <- opt) this := (value)
    }

    /**
     * Set value of slot and return this Frontlet.
     * @param value value to set.
     * @return this frontlet.
     */
    def set(value: T): thisFrontlet.type = {
      this := value
      thisFrontlet
    }

    /**
     * Set the value of this slot using an option. If parameter
     * is Some(value) the value is set, if None nothing is changed. Returns
     * this frontlet.
     * @param opt the option to use.
     * @return the encompassing frontlet.
     */
    def set(opt: Option[T]): thisFrontlet.type = {
      for (value <- opt) this := value
      thisFrontlet
    }

    override def toString = name + ":" + _map(name)
  }

  /**
   * A slot containing primitive values (ints, strings, booleans etc.).
   * @param n the name of this slot.
   * @tparam T the type of the attribute.
   */
  abstract class PrimitiveSlot[T](n: String) extends Slot[T](n) {
    def value: T = _map(name).asInstanceOf[T]

    def :=(value: T) {
      _map.update(name, value)
    }
  }

  //case class AnySlot(override val name:String) extends PrimitiveSlot[Any](name) // Too dangerous?
  case class IntSlot(override val name: String) extends PrimitiveSlot[Int](name)

  case class BooleanSlot(override val name: String) extends PrimitiveSlot[Boolean](name)

  case class DoubleSlot(override val name: String) extends PrimitiveSlot[Double](name)

  case class StringSlot(override val name: String) extends PrimitiveSlot[String](name)

  case class DateSlot(override val name: String) extends PrimitiveSlot[java.util.Date](name)

  // TODO We need other primitive types supported in BSON
  /**
   * A slot containing a list of primitives.
   * @param name the name of the slot.
   * @tparam A the type of primitives the list contains.
   */
  abstract class PrimitiveListSlot[A](override val name: String) extends Slot[Seq[A]](name) {

    import collection.JavaConversions._

    /**
     * Returns the Seq stored in the underlying map. Does some internal conversion if
     * the underlying list is not a Seq but something convertible.
     * @todo: remove the case statement here and rely on underlying map implementation
     *        to return the correct Seq representation.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: Seq[A] = _map(name) match {
      case s: Seq[A] => s
      case al: java.util.ArrayList[A] => al.toSeq
      case m: java.util.Map[String, A] => Range(0, m.size).map(i => m.get(i.toString))
      case m: mutable.Map[String, A] => m.map(_._2).toSeq
      case null => null
    }

    def :=(value: Seq[A]) {
      _map.update(name, value)
    } // TODO Perhaps we should store a Map[String,Any] here instead, like BSON?  Avoids the need for conversion later
  }

  case class IntListSlot(override val name: String) extends PrimitiveListSlot[Int](name)

  case class BooleanListSlot(override val name: String) extends PrimitiveListSlot[Boolean](name)

  case class DoubleListSlot(override val name: String) extends PrimitiveListSlot[Double](name)

  case class StringListSlot(override val name: String) extends PrimitiveListSlot[String](name)

  /**
   * A slot that contains a list of frontlets.
   * @param name the name of the slot.
   * @param constructor the frontlet constructor that will be used to create frontlets for
   *                    underyling map objects.
   * @tparam A the type of the frontlets in the list.
   */
  case class FrontletListSlot[A <: Frontlet](override val name: String, constructor: () => A) extends Slot[Seq[A]](name) {

    /**
     * Returns the list of frontlets in this slot. The underlying map is expected to
     * contain a list of maps for the field corresponding to this slot. The slot
     * will then convert the maps to frontlets using the provided constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: Seq[A] = _map(name) match {
      case null => null
      case s: Seq[AnyRef] => if (s.length == 0) Nil
      else s.map(m => {
        val c = constructor()
        c._map = m.asInstanceOf[MapType]
        c
      }) // The AnyRef is expected to be a Scala or Java Map
      //      case al:java.util.ArrayList[A] => if (al.size == 0) Nil else al.toSeq.map(m => { val c = constructor(); c._map = m; c }) // The AnyRef is expected to be a Scala or Java Map
    }

    /**
     * Set the sequence of frontlets. The slot will only store the corresponding list
     * of maps in its field.
     * @param value value to set.
     */
    def :=(value: Seq[A]) {
      _map.update(name, value.map(c => c._map))
    } // Actually put in the sequence of Maps, not sequence of Frontlets
  }


  /**
   * A RefSlot is a simple type of AbstractInverseSlot which "contains" all frontlets
   * of a given type with the given ID. Assuming uniqueness of IDs, it's a
   * unique inverse slot.
   *
   * The RefSlot field only stores the actual ID of the referenced frontlet. This means
   * that calling value or apply() on this slot returns (untyped) ids. Likewise,
   * the setter methods only take ids. Convenience methods exist to pass in
   * frontlets instead.
   *
   * @param name the name of the slot.
   * @param constructor the frontlet constructor for frontlets the slot contains.
   * @tparam A the type of frontlets this slot contains.
   */
  case class RefSlot[A <: Frontlet](override val name: String, constructor: () => A)
    extends Slot[Any](name) with AbstractRefSlot[A] with AbstractInverseSlot[A] {
    def value = _map(name)

    override def unique = true

    def foreignSlot = _.asInstanceOf[A].Id

    def target = opt

    def slot = (a: A) => a.Id

    def :=(ref: Any) {
      if (ref.isInstanceOf[Frontlet]) throw new Error("Use ::= to set RefSlot by a Frontlet");
      _map.update(name, ref)
    }

    def ::=(value: A) {
      _map.update(name, value.id)
    }
  }

  /**
   * A helper trait for libraries that need ref slots to be covariant.
   * @tparam A
   */
  trait AbstractRefSlot[+A <: Frontlet] extends AbstractSlot[Any] {

    /**
     * The value of a RefSlot is the id of the referenced frontlet. To get an actual frontlet, clients need
     * to call this method. It takes as implicit parameter a mapping from ids to frontlets. This is a deliberate
     * choice: the slot itself is not supposed to store any internal state/mapping to frontlets, it only stores
     * the id.
     * @param tr mapping from ids to frontlets.
     * @return the object associated with the given id in the given mapping.
     */
    def deref(implicit tr: scala.collection.Map[Any, Frontlet]) = tr(value).asInstanceOf[A]

    //    def ->(coll:MongoFrontletCollection[A]):GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(this,coll)
  }

  /**
   * A slot that contains a frontlet.
   * @param name the name of the slot.
   * @param constructor the constructor to create the contained frontlet wrapper with.
   * @tparam A the type of frontlet the slot contains.
   */
  case class FrontletSlot[A <: Frontlet](override val name: String, constructor: () => A) extends Slot[A](name) {
    /**
     * Return the frontlet contained in this slot. This assumes
     * that the underlying map has a corresponding field that contains a
     * map. This map is then wrapped with a frontlet created by the given constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: A = {
      val a = constructor()
      a._map = _map(name).asInstanceOf[MapType]
      a
    }

    /**
     * Stores the map underlying the passed in frontlet into the underyling map of this frontlet.
     * @param value value to set.
     */
    def :=(value: A) {
      _map.update(name, value._map)
    }
  }

}

object JSonTest {
  def main(args: Array[String]) {
    val json = JSON.parseFull("")

  }
}

// Also make a version of this that caches objects as they come out of MongoDB
@deprecated("Will be removed.")
class FrontletRefs extends scala.collection.mutable.HashMap[Any, Frontlet]


