package org.riedelcastro.frontlets

import collection.mutable
import scala.collection

/**
 * The AbstractFrontlet abstracts over the type of underlying map structure used to store the data.
 * Most importantly, this enables both mutable and immutable frontlets (using immutable maps underneath).
 * @author riedelcastro
 */
trait AbstractFrontlet {
  thisFrontlet =>

  type FrontletType <: AbstractFrontlet
  type GenericMap = collection.Map[String, Any]

  def assign(key: String, value: Any): FrontletType
  def assignTransient(key: String, value: Any): FrontletType


  def self: FrontletType

  def get(key: String): Option[Any]
  def getTransient(key: String): Option[Any]


  def setMap(map: GenericMap): FrontletType

  def addMap(map: GenericMap): FrontletType

  def +=(that:AbstractFrontlet):FrontletType = addMap(that.asMap)

  def asMap: GenericMap

  override def equals(that: Any) = that match {
    case thatFrontlet: AbstractFrontlet => this.asMap == thatFrontlet.asMap
    case _ => false
  }

  override def hashCode() = asMap.hashCode()

  override def toString = toJSON

  def toJSON = FrontletJacksonMapper.writeValueAsString(asMap)

  /**
   * Uses the json string to set the internal map.
   * @param json the json string to parse and set the map content with.
   * @return this frontlet.
   */
  def setJSON(json: String): FrontletType = {
    setMap(FrontletJacksonMapper.readValue[mutable.Map[String, Any]](json))
  }

  final def id = Id()

  /**
   * Typed access to the frontlet class.
   * @return the Frontlet class.
   */
  def frontletClass = getClass.asInstanceOf[Class[AbstractFrontlet]]

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
    def value: T = opt.get

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

    /**
     * Creates a default value for this slot
     * @return default value for this slot
     */
    def default:T

  }


  /**
   * Every frontlet has an ID. This ID is itself a field of the underlying map, and hence
   * can also be accessed through a slot.
   */
  object Id extends BasicSlot[Any] {
    def name = "_id"

    def :=(value: Any) = assign(name, value)

    def opt = get(name)

    def default = 0
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
  sealed trait AbstractInverseSlot[+A <: AbstractFrontlet] {
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
    def foreignSlot: AbstractFrontlet => A#AbstractSlot[Any]

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
  case class InverseSlot[A <: AbstractFrontlet](name: String,
                                                slot: A => A#AbstractRefSlot[AbstractFrontlet])(implicit m: Manifest[A])
    extends AbstractInverseSlot[A] {

    /**
     * This method returns some set of frontlets associated with this slot based
     * on the provided cache object.
     * @param cache A mapping from frontlet slots to iterables of frontlets. The iterable
     *              for this  frontlet slot should correspond to a set of frontlets that
     *              have the given target value for the given slot.
     * @return all frontlets that are associated with this slot in the cache.
     */
    def value(implicit cache: AbstractFrontlet#InverseSlot[AbstractFrontlet] => Iterable[AbstractFrontlet]): Iterable[A] = {
      cache(this.asInstanceOf[InverseSlot[AbstractFrontlet]]).asInstanceOf[Iterable[A]]
    }

    //todo: this is probably very slow, as I need access the manifest, erasure, create new object etc.
    def value2(implicit cache: collection.Map[(Class[AbstractFrontlet], String, Any), Iterable[AbstractFrontlet]]) = {
      val foreignFrontlet = m.erasure.newInstance().asInstanceOf[A]
      val foreignSlot = slot(foreignFrontlet)
      cache((foreignFrontlet.frontletClass, foreignSlot.name, frontlet.id)).asInstanceOf[Iterable[A]]
    }

    def foreignSlot = (c: AbstractFrontlet) => slot(c.asInstanceOf[A])

    def target = Some(frontlet.id)

    def manifest = m.asInstanceOf[Manifest[AbstractFrontlet]]

    def frontlet: thisFrontlet.type = thisFrontlet

  }

  /**
   * Default implementation of an AbstractSlot.
   * @tparam T the type of the attribute.
   */
  abstract class BasicSlot[T] extends AbstractSlot[T] {
    /**
     * Set the value for this slot.
     * @param value value to set.
     */
    def :=(value: T): FrontletType

    /**
     * Set the value and return the containing Frontlet.
     * @param value the value to set.
     * @return the frontlet this slot belongs to.
     */
    def apply(value: T): FrontletType = this := value

    def opt: Option[T]

    /**
     * Set a raw value into the underlying map. Should generally only be used
     * in other library code.
     * @param value the value to be set.
     */
    def rawPut(value: Any) = {
      assign(name, value)
    }

    /**
     * Does the frontlet have this slot.
     * @return true iff the underyling map has this slot.
     */
    def isDefined: Boolean = get(name).isDefined

    /**
     * Set the value of this slot using an option. If parameter
     * is Some(value) the value is set, if None nothing is changed.
     * @param opt the option to use.
     */
    def :=(opt: Option[T]): FrontletType = {
      opt match {
        case Some(value) => this := value
        case None => self
      }
    }

    /**
     * Set a value for this slot but inform the provided hook before this happens.
     * @param value the value to set.
     * @param preHook the hook to call before setting the value.
     */
    def :=!(value: T)(implicit preHook: Function2[AbstractFrontlet#AbstractSlot[Any], Any, Unit]) = {
      preHook(this, value)
      this := value
    }

    /**
     * Sets the slot to the slots default value
     * @return the frontlet of this slot
     */
    def setToDefault() = this := default

    override def toString = name + ":" + get(name)
  }

  abstract class Slot[T](val name: String) extends BasicSlot[T]

  case class TransientSlot[T](override val name:String, init: ()=>T) extends Slot[T](name) {

    def :=(value: T) = assignTransient(name,value)
    def opt = getTransient(name).asInstanceOf[Option[T]]
    def default = init()
  }

  /**
   * A slot containing primitive values (ints, strings, booleans etc.).
   * @param n the name of this slot.
   * @tparam T the type of the attribute.
   */
  abstract class PrimitiveSlot[T](n: String) extends Slot[T](n) {

    def opt = get(name).asInstanceOf[Option[T]]

    def :=(value: T) = {
      assign(name, value)
    }
  }

  //case class AnySlot(override val name:String) extends PrimitiveSlot[Any](name) // Too dangerous?
  case class IntSlot(override val name: String) extends PrimitiveSlot[Int](name) {
    def default = 0
  }

  case class BooleanSlot(override val name: String) extends PrimitiveSlot[Boolean](name) {
    def default = false
  }

  case class DoubleSlot(override val name: String) extends PrimitiveSlot[Double](name)  {
    def default = 0.0
  }

  case class StringSlot(override val name: String) extends PrimitiveSlot[String](name) {
    def default = ""
  }

  case class DateSlot(override val name: String) extends PrimitiveSlot[java.util.Date](name) {
    def default = new java.util.Date
  }



  /**
   * A slot containing a list of primitives.
   * @param name the name of the slot.
   * @tparam A the type of primitives the list contains.
   */
  abstract class PrimitiveListSlot[A](override val name: String) extends Slot[Seq[A]](name) {

    /**
     * Returns the Seq stored in the underlying map.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def opt: Option[Seq[A]] = get(name).asInstanceOf[Option[Seq[A]]]

    def :=(value: Seq[A]) = assign(name, value)



  }

  case class IntListSlot(override val name: String) extends PrimitiveListSlot[Int](name) {
    def default = Seq(0)
  }

  case class BooleanListSlot(override val name: String) extends PrimitiveListSlot[Boolean](name) {
    def default = Seq(false)
  }

  case class DoubleListSlot(override val name: String) extends PrimitiveListSlot[Double](name) {
    def default = Seq(0.0)
  }

  case class StringListSlot(override val name: String) extends PrimitiveListSlot[String](name) {
    def default = Seq("")
  }

  trait FrontletIterableSlot[F<:AbstractFrontlet,C <: Iterable[F]] extends AbstractSlot[C] {
    def map(f:F=>F):FrontletType = {
      assign(name,value.map(f(_).asMap))
    }
  }

  case class FrontletSeqSlot[A <: AbstractFrontlet](override val name: String, construct: Int => A)
    extends Slot[Seq[A]](name) with FrontletIterableSlot[A,Seq[A]] {

    def opt = get(name) match {
      case Some(s: Seq[_]) =>
        val frontlets = for (i <- s.indices; m = s(i)) yield
          construct(i).addMap(m.asInstanceOf[collection.Map[String, Any]])
        Some(frontlets.asInstanceOf[Seq[A]])
      case None => None
    }

    def :=(value: Seq[A]) = {
      assign(name, value.map(_.asMap))
    }

    def default = Seq(construct(0))

  }

  /**
   * A slot that contains a list of frontlets.
   * @param name the name of the slot.
   * @param constructor the frontlet constructor that will be used to create frontlets for
   *                    underyling map objects.
   * @tparam A the type of the frontlets in the list.
   */
  case class FrontletListSlot[A <: AbstractFrontlet](override val name: String, constructor: () => A)
    extends Slot[Seq[A]](name) with FrontletIterableSlot[A,Seq[A]] {

    /**
     * Returns the list of frontlets in this slot. The underlying map is expected to
     * contain a list of maps for the field corresponding to this slot. The slot
     * will then convert the maps to frontlets using the provided constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def opt = get(name) match {
      case Some(s: Seq[_]) =>
        val frontlets = s.view.map(m => constructor().addMap(m.asInstanceOf[collection.Map[String, Any]]))
        Some(frontlets.asInstanceOf[Seq[A]])
      case None => None
    }

    /**
     * Set the sequence of frontlets. The slot will only store the corresponding list
     * of maps in its field.
     * @param value value to set.
     */
    def :=(value: Seq[A]) = {
      assign(name, value.map(_.asMap))
    }

    def default = Seq(constructor())

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
  case class RefSlot[A <: AbstractFrontlet](override val name: String, constructor: () => A)
    extends Slot[Any](name) with AbstractRefSlot[A] with AbstractInverseSlot[A] {

    def opt = get(name)

    override def unique = true

    def foreignSlot = _.asInstanceOf[A].Id

    def target = opt

    def slot = (a: A) => a.Id

    def :=(ref: Any) = {
      if (ref.isInstanceOf[AbstractFrontlet]) sys.error("Use ::= to set RefSlot by a Frontlet")
      assign(name, ref)
    }

    def ::=(value: A) = {
      assign(name, value.Id())
    }

    def default = 0
  }

  /**
   * A helper trait for libraries that need ref slots to be covariant.
   * @tparam A the type of frontlet the slot refers to.
   */
  trait AbstractRefSlot[+A <: AbstractFrontlet] extends AbstractSlot[Any] {

    /**
     * The value of a RefSlot is the id of the referenced frontlet. To get an actual frontlet, clients need
     * to call this method. It takes as implicit parameter a mapping from ids to frontlets. This is a deliberate
     * choice: the slot itself is not supposed to store any internal state/mapping to frontlets, it only stores
     * the id.
     * @param tr mapping from ids to frontlets.
     * @return the object associated with the given id in the given mapping.
     */
    def deref(implicit tr: scala.collection.Map[Any, AbstractFrontlet]) = tr(value).asInstanceOf[A]

    //    def ->(coll:MongoFrontletCollection[A]):GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(this,coll)
  }

  /**
   * A slot that contains a frontlet.
   * @param name the name of the slot.
   * @param constructor the constructor to create the contained frontlet wrapper with.
   * @tparam A the type of frontlet the slot contains.
   */
  case class FrontletSlot[A <: AbstractFrontlet](override val name: String, constructor: () => A)
    extends Slot[A](name) {
    /**
     * Return the frontlet contained in this slot. This assumes
     * that the underlying map has a corresponding field that contains a
     * map. This map is then wrapped with a frontlet created by the given constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def opt = {
      for (map <- get(name)) yield constructor().setMap(map.asInstanceOf[GenericMap]).asInstanceOf[A]
    }

    /**
     * Stores the map underlying the passed in frontlet into the underyling map of this frontlet.
     * @param value value to set.
     */
    def :=(value: A) = assign(name, value.asMap)

    /**
     * Creates a frontlet of this slot's frontlet type, apply the given function to it, and set the slot
     * to be the result of that application.
     * @param f the function to apply to the created frontlet
     * @return the modified frontlet.
     */
    def create(f: A => A = identity(_)): FrontletType = {
      this := f(constructor())
    }
    /**
     * Changes the content of this slot by applying the given function to its current value.
     * If no value is assigned to the slot before, a new value is constructed.
     * @param f the function to apply to the slot value
     * @return the modified frontlet.
     */
    def apply(f: A => A): FrontletType = {
      this := f(opt.getOrElse(constructor()))
    }

    def default = constructor()

  }

}

/**
 * An immutable frontlet, backed by an immutable map. Any assignment of values creates a new
 * frontlet copy.
 * @tparam F self type, should be the implementing subclass.
 */
abstract class ImmutableFrontlet[F <: ImmutableFrontlet[F]] extends AbstractFrontlet {
  this: F =>

  type FrontletType = F
  type MapType = Map[String, Any]

  private var map: MapType = Map.empty
  private var mapTransient: MapType = Map.empty


  def construct(): F

  def +(that:FrontletType) = this += that

  def create(map: MapType) = {
    val f = construct()
    f.map = map
    f.mapTransient = mapTransient
    f
  }
  def create(map: GenericMap, mapTransient:GenericMap = Map.empty) = {
    val f = construct()
    f.map = Map.empty ++ map
    f.mapTransient = Map.empty ++ mapTransient
    f
  }

  def assign(key: String, value: Any) = {
    create(map + (key -> value))
  }

  def assignTransient(key: String, value: Any) = {
    create(Map.empty, mapTransient + (key -> value))
  }

  def getTransient(key: String) = mapTransient.get(key)
  def get(key: String) = map.get(key)

  def asMap = map

  def setMap(map: GenericMap) = {
    create(map)
  }

  def addMap(map: GenericMap) = {
    create(this.map ++ map)
  }
  def self: FrontletType = this

}

abstract class OuterFrontlet[F <: OuterFrontlet[F]] extends ImmutableFrontlet[F] {
  this: F =>
  def construct() = getClass.newInstance().asInstanceOf[F]
}

/**
 * A mutable Frontlet backed by a mutable map.
 */
class Frontlet extends AbstractFrontlet {
  type MapType = mutable.Map[String, Any]
  private var map: MapType = new mutable.HashMap[String, Any]
  private var mapTransient: MapType = new mutable.HashMap[String, Any]

  type FrontletType = this.type

  def assign(key: String, value: Any) = {
    map(key) = value
    this
  }


  def assignTransient(key: String, value: Any) = {
    mapTransient(key) = value
    this
  }

  def getTransient(key: String) = mapTransient.get(key)

  def get(key: String) = map.get(key)

  def setMap(map: GenericMap) = {
    map match {
      case m: mutable.Map[_, _] => this.map = m.asInstanceOf[MapType]
      case _ => {
        this.map = new mutable.HashMap[String, Any]
        this.map ++= map
      }
    }
    this
  }


  def addMap(map: GenericMap) = {
    this.map ++= map
    this
  }

  def self: FrontletType = this

  def asMap = map
}

abstract class InnerFrontlet[F <: InnerFrontlet[F]] extends ImmutableFrontlet[F] {
  this: F =>
  def construct() = {
    val f = getClass().getDeclaredField("$outer")
    val o = f.get(this)
    val c = getClass.getConstructor(o.getClass)
    val n = c.newInstance(o)
    n.asInstanceOf[F]
  }
}

