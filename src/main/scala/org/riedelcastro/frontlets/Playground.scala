package org.riedelcastro.frontlets

import collection.mutable
import scala.collection

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
  }

}

