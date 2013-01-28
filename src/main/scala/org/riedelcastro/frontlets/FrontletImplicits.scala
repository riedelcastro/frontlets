package org.riedelcastro.frontlets

/**
 * @author Sebastian Riedel
 */
trait FrontletImplicits {

  implicit def toSlotPath(slot:AbstractFrontlet#AbstractSlot[Any]) = SlotPath(Seq(slot))

}

object FrontletImplicits extends FrontletImplicits