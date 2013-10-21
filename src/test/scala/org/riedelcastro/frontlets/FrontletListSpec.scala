package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

/**
 * User: rockt
 * Date: 10/21/13
 * Time: 12:25 PM
 */

class FrontletListSpec extends FunSpec with MustMatchers{
  class BasicFrontlet extends Frontlet {
    val number = IntSlot("number")
  }

  class CompositeFrontlet extends Frontlet {
    val basics = FrontletListSlot("basics", () => new BasicFrontlet)
  }

  describe("A frontlet with a FrontletListSlot") {
    it ("should be able to map the values of its FrontListSlot") {
      val basic1 = new BasicFrontlet().number(5)
      val basic2 = new BasicFrontlet().number(3)
      val composite = new CompositeFrontlet().basics(List(basic1, basic2))
      composite.basics().head.number() must be(5)
      composite.basics().map(basic => basic.number := basic.number() + 1)
      composite.basics().head.number() must be(6)
      composite.basics().tail.head.number() must be(4)
    }
    it ("should be able to change values of its FrontListSlot using foreach") {
      val basic1 = new BasicFrontlet().number(5)
      val basic2 = new BasicFrontlet().number(3)
      val composite = new CompositeFrontlet().basics(List(basic1, basic2))
      composite.basics().head.number() must be(5)
      composite.basics().foreach(basic => basic.number := basic.number() + 1)
      composite.basics().head.number() must be(6)
      composite.basics().tail.head.number() must be(4)
    }
  }
}