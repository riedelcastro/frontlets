package org.riedelcastro.frontlets

import java.io.{InputStreamReader, BufferedReader}
import org.riedelcastro.nurupo.Timer


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

    val timer = new Timer

  }

}

