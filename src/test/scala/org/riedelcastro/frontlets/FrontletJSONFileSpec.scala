package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import java.io.{FileOutputStream, PrintStream, File}
import io.Source

/**
 * @author Sebastian Riedel
 */
class FrontletJSONFileSpec extends FunSpec with MustMatchers {

  import FrontletSpec._

  describe("A FrontletJSONFile") {

    def newPerson() = new Person().age(36)

    it("should store frontlets in json format in a text file") {
      val file = File.createTempFile("frontlets-write", ".json")
      file.deleteOnExit()

      val frontlets = new FrontletJSONFile(file.getAbsolutePath, () => new Person)
      frontlets += newPerson()
      frontlets += newPerson()

      val lines = Source.fromFile(file).getLines().toSeq
      lines must be(Seq(newPerson().toJSON, newPerson().toJSON))
    }
    it("should load frontlets from files in json format") {
      val file = File.createTempFile("frontlets-load", ".json")
      file.deleteOnExit()

      val os = new FileOutputStream(file)
      val out = new PrintStream(os)
      out.println(newPerson().toJSON)
      out.println(newPerson().toJSON)
      out.close()

      val frontlets = new FrontletJSONFile(file.getAbsolutePath, () => new Person())
      Seq(newPerson(),newPerson()) must be (frontlets.toSeq)


    }
  }
}
