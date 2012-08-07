package org.riedelcastro.frontlets

import java.io.{InputStreamReader, BufferedReader}


/**
 * @author riedelcastro
 */

object Playground {

  def main(args: Array[String]) {
    trait Test[T <: Test[T]] {
      def create(): T
    }
    class A extends Test[A] {
      def create() = new A
    }
    class B extends Test[B] {
      def create() = getClass.getConstructor().newInstance().asInstanceOf[B]
    }
    class Outer {
      val inner = new Inner
      def create() = {
        println(getClass.isMemberClass)
//        println(getClass.isLocalClass)

      }
      class Inner {
        def create() = {
          println(getClass.isMemberClass)
//          println(getClass.isAnonymousClass)

          val f = getClass().getDeclaredField("$outer")
          val o = f.get(this)
          val c = getClass.getConstructor(o.getClass)
          val n = c.newInstance(o)
          println(n)
          println(f)
          println(o)
          n
        }
      }
    }
    val outer = new Outer
    outer.inner.create()
    outer.create()

    try {
      import collection.JavaConversions._
      for ((key,value) <- System.getenv()) {
        println("%30s %s".format(key,value))
      }
      val p = Runtime.getRuntime.exec("mongod")
      val stdInput = new BufferedReader(new
          InputStreamReader(p.getInputStream()))

      val stdError = new BufferedReader(new
          InputStreamReader(p.getErrorStream()))
      println("Here is the standard output of the command:\n")

      var s:String = null
      while ({s = stdInput.readLine();s} != null) {
        println(s)
      }

      // read any errors from the attempted command
      println("Here is the standard error of the command (if any):\n")
      while ({s = stdError.readLine();s} != null) {
        println(s)
      }

    } catch {
      case e: Throwable => e.printStackTrace()
    }

  }

}

