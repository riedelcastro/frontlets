package org.riedelcastro.frontlets

import scala.collection.mutable.ArrayBuffer


/**
 * @author Sebastian Riedel
 */
class CLIFrontlet[This <:CLIFrontlet[This]] extends OuterFrontlet[This]{
  this: This =>

  private var _cliSlots = List.empty[CLISlot[_,_]]

  def cliSlots = _cliSlots

  case class CLISlot[S<:BasicSlot[T],T](slot:S,optional:Boolean = true, short:String = "", help:String = "") extends BasicSlot[T] {
    def name = slot.name
    def opt = slot.opt
    def default = slot.default
    def :=(value: T) = slot := value
    _cliSlots ::= this

  }
//
//  case class CLISlotBuilder[S<:AbstractSlot[T],T](slot:S) {
//
//  }
//
  implicit def toCLISlotBuilder[T](slot:BasicSlot[T]) = new {
    def cli(key:String = "", optional:Boolean = true) = CLISlot[BasicSlot[T],T](slot,short = key, optional = optional)
  }
  implicit def toCLISlotBuilder2[T,S<:BasicSlot[T]](slot:S) = new {
    def cli2(key:String) = CLISlot[S,T](slot,short = key)
  }
//
  def cli[S<:BasicSlot[T],T](slot:S,optional:Boolean=true,short:String="",help:String="") = CLISlot[S,T](slot,optional,short, help)

  def cli2[T](slot:BasicSlot[T]) =  CLISlot[BasicSlot[T],T](slot)


}

object CLIImplicits {
  //implicit def toCLISlotBuilder[S<:AbstractFrontlet#AbstractSlot[T],T]
}

object CLIParser {
  def parse[F<:CLIFrontlet[F]](args:Array[String],params: => F) = {
    val result = params
    for (cliSlot <- result.cliSlots) {

    }
    result
  }
}