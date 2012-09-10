package org.riedelcastro.frontlets

import java.io.{FileInputStream, PrintStream, File}
import collection.JavaConversions._
import collection.mutable
import org.riedelcastro.nurupo.Util

/**
 * A file that stores frontlet in json format.
 * @author Sebastian Riedel
 */
class FrontletJSONFile[F<:AbstractFrontlet](filename:String, constructor: () => F) extends collection.mutable.Iterable[F#FrontletType] {

  private lazy val file = new File(filename)
  private lazy val out = new PrintStream(file)

  def iterator = {
    val parser = FrontletJacksonMapper.mapper.getJsonFactory.createJsonParser(file)
    val maps = Util.untilException(FrontletJacksonMapper.readValue[mutable.Map[String, Any]](parser), t => true)
    maps.map(constructor().setMap(_))
//    FrontletJacksonMapper.readValues[mutable.Map[String, Any]](new FileInputStream(file)).map(constructor().setMap(_))
  }

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
