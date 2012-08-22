package org.riedelcastro.frontlets.programs

import org.apache.bcel.generic._
import org.apache.bcel.{Repository, Constants}
import tools.nsc.util.ScalaClassLoader.URLClassLoader
import java.net.URL
import scala.collection
import collection.mutable
import collection.mutable.ArrayBuffer
import org.riedelcastro.frontlets.{Frontlet, AbstractFrontlet}
import org.apache.bcel.verifier.Verifier

/**
 * @author riedelcastro
 */
trait State {
  def get[T](variable: Var[T]): Option[T]
}

object State {
  def apply(map: collection.Map[Var[Any], Any]): State = new State {
    def get[T](variable: Var[T]) = map.get(variable).map(_.asInstanceOf[T])
  }
}

trait Eval {
  def apply[T](term: Term[T], state: State): Option[T]
}

object Eval extends Eval {
  def apply[T](term: Term[T], state: State) = term.eval(state)

  def apply(terms: Seq[Term[Any]], values: Seq[AnyRef]): Eval = new Eval {
    lazy val map = (terms.zip(values)).toMap

    def apply[T](term: Term[T], state: State) = map.get(term).asInstanceOf[Option[T]]
  }
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
  require(args.size > 1)

  def eval(state: State, eval: Eval) = Util.allOrNone(args.map(eval(_, state))).map(_.sum)

  override def children = args

  def prototype = 0
}

case class ProxyTerm[+T](self: Term[T]) extends Term[T] {
  def eval(state: State, eval: Eval) = self.eval(state)

  def prototype = self.prototype

  override def children = self +: self.children
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

class ProgramInfo(val boundVariables: Array[Var[Any]],
                  val freeVariables: Array[Var[Any]],
                  val allTerms: Array[Term[Any]]) {

}

object Compiler {

  import Constants._

  //helpers
  val N_CLASS = "SomeName"
  val N_TERM = classOf[Term[Any]].getName
  val N_CONST = classOf[Const[Any]].getName
  val N_HASHMAP = classOf[scala.collection.mutable.HashMap[Any, Any]].getName
  val N_MMAP = classOf[scala.collection.mutable.Map[Any, Any]].getName
  val N_MAP = classOf[scala.collection.Map[Any, Any]].getName
  val N_EXE = classOf[Executable].getName
  val N_STATE_OBJ = State.getClass.getName
  val N_EVAL_OBJ = Eval.getClass.getName
  val N_OPTION = classOf[Option[Any]].getName
  val N_SEQ = classOf[Seq[Any]].getName

  val N_INFO_FIELD = "info"
  val N_PROGRAM_INFO = classOf[ProgramInfo].getName
  val N_VAR = classOf[Var[Any]].getName
  val N_BOUND_VARIABLES = "boundVariables"
  val N_FREE_VARIABLES = "freeVariables"
  val N_INTEGER = classOf[java.lang.Integer].getName
  val N_ARRAYBUFFER = classOf[ArrayBuffer[Any]].getName


  val T_TERM = new ObjectType(N_TERM)
  val T_CONST = new ObjectType(N_CONST)
  val T_STATE = new ObjectType(classOf[State].getName)
  val T_PROGRAM_INFO = new ObjectType(N_PROGRAM_INFO)
  val T_STATE_OBJ = new ObjectType(N_STATE_OBJ)
  val T_EVAL_OBJ = new ObjectType(N_EVAL_OBJ)
  val T_EVAL = new ObjectType(classOf[Eval].getName)

  val T_MAP = new ObjectType(N_MAP)
  val T_VAR = new ObjectType(N_VAR)
  val T_INTEGER = new ObjectType(N_INTEGER)
  val T_OPTION = new ObjectType(N_OPTION)
  val T_ARRAYBUFFER = new ObjectType(N_ARRAYBUFFER)
  val T_SEQ = new ObjectType(N_SEQ)


  class CompilationInfo(val info: ProgramInfo) {
    private var currentLocalVarIndex = 2

    def allocateLocalVariableIndex() = {
      val old = currentLocalVarIndex
      currentLocalVarIndex += 1
      old
    }

    lazy val resultMapLocalIndex = allocateLocalVariableIndex()
    lazy val bound2InfoIndex = info.boundVariables.zipWithIndex.toMap
    lazy val free2InfoIndex = info.freeVariables.zipWithIndex.toMap
    lazy val term2InfoIndex = info.allTerms.zipWithIndex.toMap

    def appendLoadResultMap(il: InstructionList) {
      il.append(new ALOAD(resultMapLocalIndex))
    }

    def appendLoadResultState(il: InstructionList, f: InstructionFactory) {
      il.append(f.createGetStatic(N_STATE_OBJ, "MODULE$", T_STATE_OBJ))
      appendLoadResultMap(il)
      il.append(f.createInvoke(N_STATE_OBJ, "apply", T_STATE, Array(T_MAP), INVOKEVIRTUAL))
    }

  }


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

  def appendCreateObjectArray(il: InstructionList, f: InstructionFactory, elements: Seq[InstructionList => Unit], cast: Type) {
    il.append(new ICONST(elements.size))
    il.append(f.createNewArray(Type.OBJECT, 1))
    for ((element, index) <- elements.zipWithIndex) {
      il.append(new DUP)
      il.append(new ICONST(index))
      element(il)
      il.append(new AASTORE)
    }
    if (cast != Type.OBJECT) {
      il.append(f.createCast(Type.OBJECT, new ArrayType(cast, 1)))
    }
  }

  def appendCreateObjectSeq(il: InstructionList, f: InstructionFactory, elements: Seq[InstructionList => Unit]) {
    il.append(f.createNew(N_ARRAYBUFFER))
    il.append(new DUP)
    il.append(f.createInvoke(N_ARRAYBUFFER, "<init>", Type.VOID, Array.empty, INVOKESPECIAL))
    for ((element, index) <- elements.zipWithIndex) {
      element(il)
      il.append(f.createInvoke(N_ARRAYBUFFER, "$plus$eq", T_ARRAYBUFFER, Array(Type.OBJECT), INVOKEVIRTUAL))
    }
  }


  def appendCreateEval(il: InstructionList, f: InstructionFactory,
                       terms: Seq[InstructionList => Unit], values: Seq[InstructionList => Unit]) {
    il.append(f.createGetStatic(N_EVAL_OBJ, "MODULE$", T_EVAL_OBJ))
    appendCreateObjectSeq(il, f, terms)
    appendCreateObjectSeq(il, f, values)
    il.append(f.createInvoke(N_EVAL_OBJ, "apply", T_EVAL, Array(T_SEQ, T_SEQ), INVOKEVIRTUAL))
  }

  def appendInfoConstructor(cg: ClassGen, cp: ConstantPoolGen) {
    val il = new InstructionList()
    val f = new InstructionFactory(cg)
    val mg = new MethodGen(ACC_PUBLIC, Type.VOID, Array[Type](T_PROGRAM_INFO), Array("info"), "<init>", N_CLASS, il, cp)
    il.append(new ALOAD(0))
    il.append(f.createInvoke("java.lang.Object", "<init>", Type.VOID, Array.empty, INVOKESPECIAL))
    il.append(new ALOAD(0))
    il.append(new ALOAD(1))
    il.append(f.createPutField(N_CLASS, N_INFO_FIELD, T_PROGRAM_INFO))
    il.append(new RETURN)
    mg.setMaxLocals()
    mg.setMaxStack()
    cg.addMethod(mg.getMethod)
  }

  def appendThis(il: InstructionList) {
    il.append(new ALOAD(0))
  }

  def compile(program: Program): Executable = {

    //get all terms
    val terms = program.commands.collect({ case Assignment(_, term) => term })
    val allTerms = terms.flatMap(_.all)
    //    val allObjectConstants = allTerms.collect({case Const(value) =>  })

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

    //info used in the executable
    val programInfo = new ProgramInfo(bound.toArray, free.toArray, allTerms.toArray)

    def appendVariable(il: InstructionList, f: InstructionFactory, variable: Var[Any], method: String, index: Int) {
      appendGetProgramInfo(il, f)
      il.append(f.createInvoke(N_PROGRAM_INFO, method, new ArrayType(T_VAR, 1), Array.empty, INVOKEVIRTUAL))
      il.append(new ICONST(index))
      il.append(new AALOAD)
    }

    def appendTerm(il: InstructionList, f: InstructionFactory, index: Int) {
      appendGetProgramInfo(il, f)
      il.append(f.createInvoke(N_PROGRAM_INFO, "allTerms", new ArrayType(T_TERM, 1), Array.empty, INVOKEVIRTUAL))
      il.append(new ICONST(index))
      il.append(new AALOAD)
    }

    def appendConstant(il: InstructionList, f: InstructionFactory, index: Int) {
      appendTerm(il, f, index)
      il.append(f.createCast(T_TERM, T_CONST))
      il.append(f.createInvoke(classOf[Const[Any]].getName, "value", Type.OBJECT, Array.empty, INVOKEVIRTUAL))
    }


    //get all access prototypes
    val accessPrototypes = allTerms.flatMap(accessPrototype(_))
    val var2accessPrototype = accessPrototypes.groupBy(_.v).mapValues(_.reduce(_ + _))
    println(var2accessPrototype)

    println("Prototypes: ")
    for (term <- allTerms) {
      println("%-30s %s".format(term, term.prototype))
    }
    println("****")


    //build the actual executable
    val cg = new ClassGen(N_CLASS, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, Array(N_EXE))
    val cp = cg.getConstantPool
    val infoField = new FieldGen(ACC_PUBLIC | ACC_FINAL, T_PROGRAM_INFO, N_INFO_FIELD, cp).getField
    cg.addField(infoField)

    val il = new InstructionList()
    val f = new InstructionFactory(cg)
    val mg = new MethodGen(ACC_PUBLIC, T_STATE, Array[Type](T_STATE), Array("input"), "execute", N_CLASS, il, cp)


    val compilationInfo = new CompilationInfo(programInfo)
    import compilationInfo._


    //create an empty hashmap
    appendCreateEmptyHashMap(il, f)
    il.append(new ASTORE(resultMapLocalIndex))

    //remember variable to local index mapping
    val var2LocalIndex = new mutable.HashMap[Var[Any], Int]

    def appendBoxedCompiledTerm[T](term: Term[T], il: InstructionList, f: InstructionFactory) {
      appendCompiledTerm(term, il, f)
      term.prototype match {
        case i: Int => appendBox(il, f, T_INTEGER)
        case _ =>
      }
    }

    def appendGenericCompiledTerm[T](term: Term[T], il: InstructionList, f: InstructionFactory) {
      //figure out result type (this is boxed!)
      val resultType = new ObjectType(term.prototype.getClass.getName)

      //put term object on stack
      appendTerm(il, f, term2InfoIndex(term))
      //put current state on stack
      appendLoadResultState(il, f)
      //put eval object on stack
      appendCreateEval(il, f,
        term.children.map((t: Term[Any]) => (list: InstructionList) => appendTerm(list, f, term2InfoIndex(t))),
        term.children.map((t: Term[Any]) => (list: InstructionList) => appendBoxedCompiledTerm(t, list, f)))
      //call top.eval
      il.append(f.createInvoke(N_TERM, "eval", T_OPTION, Array(T_STATE, T_EVAL), INVOKEINTERFACE))

      //call top.get
      il.append(f.createInvoke(N_OPTION, "get", Type.OBJECT, Array.empty, INVOKEVIRTUAL))

      //cast
      il.append(f.createCast(Type.OBJECT, resultType))

      //unbox if necessary
      appendUnbox(resultType, il, f)
    }

    def appendCompiledTerm[T](term: Term[T], il: InstructionList, f: InstructionFactory) {

      def unwrap(term: Term[T]) = term match {
        case ProxyTerm(self) => self
        case _ => term
      }

      unwrap(term) match {

        // Append the value of the constant
        case c@Const(value) => value match {
          case i: Int => il.append(new ICONST(i))
          case _ => appendConstant(il, f, term2InfoIndex(c))
        }

        // Summing integers
        case IntSum(args) =>
          for (arg <- args) appendCompiledTerm(arg, il, f)
          for (_ <- 0 until (args.size - 1)) il.append(new IADD)

        // Call out generic eval method
        case _ =>
          appendGenericCompiledTerm(term, il, f)
      }
    }

    for (command <- program.commands) command match {
      case Assignment(variable, term) =>
        //default action: evaluate term by evaluating subterms, then create Eval object, and then call term.eval on it
        //this requires created the current input state (arguments plus local)
        //get the map to call update on
        appendLoadResultMap(il)
        appendVariable(il, f, variable, N_BOUND_VARIABLES, bound2InfoIndex(variable))
        //need to append a reference to the variable
        appendCompiledTerm(term, il, f)
        //duplicate to store both in local variable and in result map

        //this stores the result in the local variable for later use
        il.append(new DUP)
        val localVarIndex = var2LocalIndex.getOrElseUpdate(variable, allocateLocalVariableIndex())
        variable.prototype match {
          case i: Int =>
            //store result in local variable
            il.append(new ISTORE(localVarIndex))
            //store result in map
            appendBoxInt(il, f)
            appendUpdateMap(il, f)
          case _ =>
            il.append(new ASTORE(localVarIndex))
            appendUpdateMap(il, f)
        }

      //this stores the result in the map
      //il.append()
    }

    //create the return state
    appendLoadResultState(il, f)

    //create return
    il.append(new ARETURN)

    //done
    mg.setMaxStack()
    mg.setMaxLocals()
    cg.addMethod(mg.getMethod)

    appendInfoConstructor(cg, cp)

    //    cg.addEmptyConstructor(ACC_PUBLIC)

    val c = cg.getJavaClass

    println(cg)
    for (method <- cg.getMethods) {
      println(method)
      println(method.getCode.toString)
    }
    c.dump("/tmp/Generated.class")

    Repository.addClass(c)

    Verifier.main(Array(N_CLASS))

    val loader = new ByteArrayClassLoader(Map(N_CLASS -> c.getBytes), Seq.empty, this.getClass.getClassLoader)
    val exeClass = loader.findClass(N_CLASS)
    val exe = exeClass.getConstructor(classOf[ProgramInfo]).newInstance(programInfo).asInstanceOf[Executable]
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


  def appendUnbox[T](resultType: ObjectType, il: InstructionList, f: InstructionFactory) {
    resultType match {
      case t if (t == T_INTEGER) => appendUnboxInteger(il, f)
      case _ =>
    }
  }

  def appendUpdateMap(il: InstructionList, f: InstructionFactory): InstructionHandle = {
    il.append(f.createInvoke(classOf[mutable.MapLike[Any, Any, Any]].getName, "update", Type.VOID, Array(Type.OBJECT, Type.OBJECT), INVOKEINTERFACE))
  }

  def appendBoxInt(il: InstructionList, f: InstructionFactory) {
    il.append(f.createInvoke(N_INTEGER, "valueOf", T_INTEGER, Array(Type.INT), INVOKESTATIC))
  }

  def appendBox(il: InstructionList, f: InstructionFactory, target: Type) {
    target match {
      case t if (t == T_INTEGER) =>
        il.append(f.createInvoke(N_INTEGER, "valueOf", T_INTEGER, Array(Type.INT), INVOKESTATIC))
    }
  }


  def appendUnboxInteger(il: InstructionList, f: InstructionFactory) {
    il.append(f.createInvoke(N_INTEGER, "intValue", Type.INT, Array.empty, INVOKEVIRTUAL))
  }

  def appendGetProgramInfo(il: InstructionList, f: InstructionFactory) {
    appendThis(il)
    il.append(f.createGetField(N_CLASS, N_INFO_FIELD, T_PROGRAM_INFO))
  }

  def appendCreateEmptyHashMap(il: InstructionList, f: InstructionFactory) {
    il.append(f.createNew(N_HASHMAP))
    il.append(new DUP)
    il.append(f.createInvoke(N_HASHMAP, "<init>", Type.VOID, Array.empty, INVOKESPECIAL))
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
    val simpleProgram = Program(Seq(
      //      x := Const(5) + 5
      //      z := new Person().age(35),
      y := Const(new Person().age(35))(_.age)
    ))

    val exe = compile(simpleProgram)
    val result = exe.execute(State(Map.empty))
    println("Result: " + result)
    println("x: " + result.get(x))
    println("y: " + result.get(y))
    println("z: " + result.get(z))

    for (term <- Person(_.spouse, Person(_.age, y))(_.age, u(_.age)).all) {
      println("%-20s %s".format(term, accessPrototype(term)))
    }

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
