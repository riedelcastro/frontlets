package org.riedelcastro.frontlets.programs

import org.apache.bcel.generic._
import org.apache.bcel.Constants
import tools.nsc.util.ScalaClassLoader.URLClassLoader
import java.net.URL
import scala.collection
import scala.collection
import scala.collection.immutable.HashMap

/**
 * @author riedelcastro
 */
object FrontletCompiler {

  import org.riedelcastro.frontlets.Playground.Person
  import Constants._

  class ByteArrayClassLoader(val bytes: Map[String, Array[Byte]], urls: Seq[URL] = Seq.empty, parent: ClassLoader)
    extends URLClassLoader(urls, parent) {
    override def findClass(name: String) = {
      bytes.get(name).map(b => defineClass(name, b, 0, b.length)).getOrElse(super.findClass(name))
    }
  }

  def compile() {
    //find free variables in program
    //find datatype of free variables (including frontlet prototype, arrays/seq etc.)
    //find datatype of result variables
    //create local variables and private fields in the executable corresponding to the variables found
    //create fromMap and toMap methods that initialize the variables from the input map, and create the output map.
    //create commands that assign correct values to fields and variables
  }


  def main(args: Array[String]) {
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
    val loader = new ByteArrayClassLoader(Map("Person" -> c.getBytes), Seq.empty, getClass.getClassLoader)
    val personClass = loader.findClass("Person")
    val newPerson = personClass.getConstructor(classOf[Int]).newInstance(new Integer(36))

  }
}

trait FrontletExecutable {
  def execute(input:collection.Map[String,Any]):collection.Map[String,Any]
}

class SimpleExecutable extends FrontletExecutable {
  private var age = 0
  private var result = 0

  def execute(input: collection.Map[String, Any]) = {
    age = input("age").asInstanceOf[Int]
    result = age * age

    val output = new collection.mutable.HashMap[String,Any]
    output("result") = result
    output
  }
}

class PersonTest {
  var i:Int = 1
  def this(j:Int) {
    this()
    i = j
  }
}
