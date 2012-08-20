package org.riedelcastro.frontlets.programs

import org.apache.bcel.generic._
import org.apache.bcel.Constants
import tools.nsc.util.ScalaClassLoader.URLClassLoader
import java.net.URL
import scala.collection
import collection.mutable
import org.riedelcastro.frontlets.{Frontlet, AbstractFrontlet}

/**
 * @author riedelcastro
 */
trait State {
  def get[T](variable: Var[T]): Option[T]
}

object State {
  def apply(map: collection.Map[Var[Any], Any]) = new State {
    def get[T](variable: Var[T]) = map.get(variable).map(_.asInstanceOf[T])
  }
}

trait Eval {
  def apply[T](term: Term[T], state: State): Option[T]
}

object Eval extends Eval {
  def apply[T](term: Term[T], state: State) = term.eval(state)
}

sealed trait Term[+T] {
  def eval(state: State, eval: Eval = Eval): Option[T]

  def prototype: T

  def children: Seq[Term[Any]] = Seq.empty

  def all: Seq[Term[Any]] = this +: children.flatMap(_.all)

}

case class Binding[+T](variable: Var[T], value: T)

trait Var[+T] extends Term[T] {
  def name: String

  def eval(state: State, eval: Eval) = state.get(this)
}

case class FrontletVar[F <: AbstractFrontlet](name: String, constructor: () => F) extends FrontletTerm[F] with Var[F] {
  def prototype = constructor()
}

case class SimpleVar[+T](name: String, default: T) extends Var[T] {
  def prototype = default
}

trait FrontletTerm[F <: AbstractFrontlet] extends Term[F] {
  def apply[T](slot: F => F#Slot[T]) = Get(this, slot)

  def apply[T](slot: F => F#Slot[T], value: Term[T]) = Set(this, slot, value)
}

trait IntTerm extends Term[Int] {
  def +(that: Term[Int]) = IntSum(Seq(this, that))
}

case class IntSum(args: Seq[Term[Int]]) extends IntTerm {
  def eval(state: State, eval: Eval) = Util.allOrNone(args.map(eval(_, state))).map(_.sum)

  override def children = args

  def prototype = 0
}

case class ProxyTerm[+T](self: Term[T]) extends Term[T] with Proxy {
  def eval(state: State, eval: Eval) = self.eval(state)

  def prototype = self.prototype
}

case class Assignment[+T](variable: Var[T], term: Term[T]) extends Command

trait Command

case class Program(commands: Seq[Command])

case class Const[+T](value: T) extends Term[T] {
  def eval(state: State, eval: Eval) = Some(value)

  def prototype = value
}

case class Get[F <: AbstractFrontlet, T](frontlet: Term[F], slot: F => F#Slot[T]) extends Term[T] {
  def eval(state: State, eval: Eval) = eval(frontlet, (state)).map(slot(_).value)

  override def children = Seq(frontlet)

  def prototype = slot(frontlet.prototype).default
}

case class Set[F <: AbstractFrontlet, T](frontlet: Term[F], slot: F => F#Slot[T], value: Term[T])
  extends FrontletTerm[F#FrontletType] {
  def eval(state: State, eval: Eval) = for (f <- frontlet.eval(state); v <- eval(value, state)) yield slot(f) := v

  override def children = Seq(frontlet, value)

  def prototype = slot(frontlet.prototype).setToDefault()
}

object Util {
  def allOrNone[T, V](args: Iterable[Option[T]]): Option[Iterable[T]] = {
    if (args.forall(_.isDefined)) Some(args.map(_.get)) else None
  }
}

class Person extends Frontlet {
  val age = IntSlot("age")
  val spouse = FrontletSlot("spouse", () => new Person)
}

object TermImplicits {
  implicit def toFrontletTerm[F <: AbstractFrontlet](term: Term[F]) = new ProxyTerm(term) with FrontletTerm[F]

  implicit def termToIntTerm(term: Term[Int]) = new ProxyTerm(term) with IntTerm

  implicit def valueToTerm[T](value: T) = Const(value)

  implicit def toAssignmentBuilder[T](v: Var[T]) = new AnyRef {
    def :=(that: Term[T]) = Assignment(v, that)
  }

}

object Example {
  def main(args: Array[String]) {

    import TermImplicits._

    val p = FrontletVar("p", () => new Person)
    val silly = p(_.age, p(_.spouse)(_.age))
    val sum = p(_.age) + p(_.age) + 5
    p := p(_.spouse)

    val person = new Person().age(36)
    val state = State(Map(p -> person))
    println(sum.eval(state))

  }
}

object Compiler {

  import Constants._

  case class AccessPrototype(v: Var[AbstractFrontlet], proto: AbstractFrontlet) {
    def +(that: AccessPrototype) = copy(proto = proto += that.proto)
  }

  def accessPrototype(term: Term[Any]): Option[AccessPrototype] = {
    term match {
      case Get(f, s) => for (a <- accessPrototype(f)) yield a.copy(proto = s(a.proto).setToDefault())
      case v@FrontletVar(name, constructor) => Some(AccessPrototype(v, constructor()))
      case _ => None
    }
  }

  class ByteArrayClassLoader(val bytes: Map[String, Array[Byte]], urls: Seq[URL] = Seq.empty, parent: ClassLoader)
    extends URLClassLoader(urls, parent) {
    override def findClass(name: String) = {
      bytes.get(name).map(b => defineClass(name, b, 0, b.length)).getOrElse(super.findClass(name))
    }
  }

  def compile(program: Program): Executable = {

    //get all terms
    val terms = program.commands.collect({ case Assignment(_, term) => term })
    val allTerms = terms.flatMap(_.all)

    //get free and bound variables
    val bound = new mutable.HashSet[Var[Any]]
    val free = new mutable.HashSet[Var[Any]]
    for (command <- program.commands) command match {
      case Assignment(variable, term) =>
        bound(variable) -> free(variable) match {
          case (_, true) => sys.error("Free variable should not be bound")
          case (true, false) => sys.error("Variable should not be bound twice")
          case (false, false) => bound += variable
        }
        for (v <- term.all.collect({ case v: Var[Any] => v })) bound(v) match {
          case false => free += v
          case true => if (v == variable) sys.error("Can't use recursion in assignment of variable")
        }
    }
    println(bound)
    println(free)

    //get all access prototypes
    val accessPrototypes = allTerms.flatMap(accessPrototype(_))
    val var2accessPrototype = accessPrototypes.groupBy(_.v).mapValues(_.reduce(_ + _))
    println(var2accessPrototype)

    println("Prototypes: ")
    for (term <- allTerms) {
      println("%-30s %s".format(term, term.prototype))
    }
    println("****")

    //helpers
    val N_CLASS = "SomeName"
    val T_STATE = new ObjectType(classOf[State].getName)
    val N_EXE = classOf[Executable].getName

    //build the actual executable
    val cg = new ClassGen(N_CLASS, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, Array(N_EXE))
    val cp = cg.getConstantPool

    val il = new InstructionList()
    val f = new InstructionFactory(cg)
    val mg = new MethodGen(ACC_PUBLIC, T_STATE, Array[Type](T_STATE), Array("input"), "execute", N_CLASS, il, cp)
    il.append(new ACONST_NULL)
    il.append(f.createCast(Type.NULL,T_STATE))
    il.append(new ARETURN)
    mg.setMaxStack()
    cg.addMethod(mg.getMethod)

    cg.addEmptyConstructor(ACC_PUBLIC)

    val c = cg.getJavaClass


    val loader = new ByteArrayClassLoader(Map(N_CLASS -> c.getBytes), Seq.empty, this.getClass.getClassLoader)
    val exeClass = loader.findClass(N_CLASS)
    val exe = exeClass.getConstructor().newInstance().asInstanceOf[Executable]
    exe



    //for each frontlet variable get a requirement prototype (based on getters)

    //for each frontlet term get a prototype


    //find free variables in program
    //find datatype of free variables (including frontlet prototype, arrays/seq etc.)
    //find datatype of result variables
    //create local variables and private fields in the executable corresponding to the variables found
    //create fromMap and toMap methods that initialize the variables from the input map, and create the output map.
    //create commands that assign correct values to fields and variables
  }


  def main(args: Array[String]) {

    import TermImplicits._
    val x = SimpleVar("x", 0)
    val y = SimpleVar("y", 0)
    val z = FrontletVar("z", () => new Person)
    val u = FrontletVar("u", () => new Person)
    def Person = Const(new Person)
    val program = Program(Seq(
      y := x,
      z := Person(_.spouse, Person(_.age, y))(_.age, u(_.age))
    ))
    val exe = compile(program)

    println("Result: " + exe.execute(State(Map.empty)))

    for (term <- Person(_.spouse, Person(_.age, y))(_.age, u(_.age)).all) {
      println("%-20s %s".format(term, accessPrototype(term)))
    }


    val person = new Person().age(36)
    val cg = new ClassGen("Person", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp = cg.getConstantPool
    val ageField = new FieldGen(ACC_PUBLIC | ACC_FINAL, Type.INT, "age", cp).getField
    cg.addField(ageField)

    val il = new InstructionList()
    val f = new InstructionFactory(cg)
    val mg = new MethodGen(ACC_PUBLIC,
      Type.VOID, Array[Type](Type.INT), Array("age"), "<init>", "Person", il, cp)

    il.append(new ALOAD(0))
    il.append(f.createInvoke("java.lang.Object", "<init>", Type.VOID, Array.empty, INVOKESPECIAL))
    il.append(new ALOAD(0))
    il.append(new ILOAD(1))
    il.append(f.createPutField("Person", "age", Type.INT))
    il.append(new RETURN)

    mg.setMaxStack()
    cg.addMethod(mg.getMethod)

    val c = cg.getJavaClass
    c.dump("/tmp/Person.class")
    val loader = new ByteArrayClassLoader(Map("Person" -> c.getBytes), Seq.empty, this.getClass.getClassLoader)
    val personClass = loader.findClass("Person")
    val newPerson = personClass.getConstructor(classOf[Int]).newInstance(new Integer(36))

  }
}

trait Executable {
  def execute(input: State): State
}

class SimpleExecutable extends Executable {
  private var age = 0
  private var result = 0

  def execute(input: State) = {
    val map = new collection.mutable.HashMap[Var[Any], Any]
    map(SimpleVar("age", 0)) = age
    State(map)
  }
}

class PersonTest {
  var i: Int = 1

  def this(j: Int) {
    this()
    i = j
  }
}
