package org.riedelcastro.frontlets

import java.io.{FileInputStream, InputStream, PrintStream, File}
import collection.mutable
import org.riedelcastro.nurupo.Util

/**
 * A file that stores frontlet in json format.
 * @author Sebastian Riedel
 */
class FrontletJSONFile[F<:AbstractFrontlet](filename:String, constructor: () => F) extends collection.mutable.Iterable[F#FrontletType] {

  private lazy val file = new File(filename)
  private lazy val out = new PrintStream(file)

  def iterator = FrontletIO.fromJSONFile(new FileInputStream(file),constructor)

  def +=(f:F) {
    out.println(f.toJSON)
  }

  def close() {
    out.close()
  }

  def flush() {
    out.flush()
  }

}

/**
 * IO Helpers for frontlets.
 */
object FrontletIO {
  def fromJSONFile[F<:AbstractFrontlet](in:InputStream, constructor:() => F):Iterator[F#FrontletType] = {
    val parser = FrontletJacksonMapper.mapper.getJsonFactory.createJsonParser(in)
    val maps = Util.untilException(FrontletJacksonMapper.readValue[mutable.Map[String, Any]](parser), t => true)
    maps.map(constructor().setMap(_))
  }

  def printToJSONFile[F<:AbstractFrontlet](out:PrintStream, frontlets:TraversableOnce[F]) = {
    for (f <- frontlets) out.println(f.toJSON)
  }


}