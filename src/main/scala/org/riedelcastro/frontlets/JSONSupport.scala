package org.riedelcastro.frontlets


import com.lambdaworks.jacks._
import com.fasterxml.jackson.databind.deser.std.{UntypedObjectDeserializer => BaseUntypedObjectDeserializer}
import collection._
import generic._
import com.fasterxml.jackson.databind._
import deser.Deserializers
import java.io._
import java.util.concurrent.ConcurrentHashMap
import scala.Some
import collection.JavaConversions.JConcurrentMapWrapper
import com.fasterxml.jackson.core.{JsonParser, Version}
import org.bson.types.ObjectId

case class A(i:Int)
case class B(a:A)

/**
 * @author riedelcastro
 */
object JSONSupport {

  def main(args: Array[String]) {
    val json = """{"one":1,"two":{"key":5}}"""
    val parsedJacks = JacksMapper.readValue[collection.mutable.Map[String,Any]](json)
    val parsedFrontlet = FrontletJacksonMapper.readValue[collection.mutable.Map[String,Any]](json)
//    val parsed = parse[collection.mutable.Map[String, Any]](json) //=> Map("one"->1,"two"->2)
    println(parsedJacks)
    println(parsedFrontlet)


    val b = B(A(3))
    val bJSON = FrontletJacksonMapper.writeValueAsString(b)
    println(bJSON)
    println(FrontletJacksonMapper.readValue[B](bJSON))

    val t = new Frontlet
    t.Id := new ObjectId()
    println(t.toJSON)


  }

}

class FrontletJacksonModule extends Module {
  def version       = new Version(0, 2, 0, null, "org.riedelcastro.frontlets", "frontlets")
  def getModuleName = "ScalaModule"

  def setupModule(ctx: Module.SetupContext) {
    ctx.addSerializers(new ScalaSerializers)
    ctx.addDeserializers(new FrontletJacksonDeserializers)
  }
}

class FrontletJacksonDeserializers extends Deserializers.Base {
  override def findBeanDeserializer(t: JavaType, cfg: DeserializationConfig, bd: BeanDescription): JsonDeserializer[_] = {
    val cls = t.getRawClass

    if (classOf[GenTraversable[_]].isAssignableFrom(cls)) {
      if (classOf[GenSeq[_]].isAssignableFrom(cls)) {
        val c = companion[GenericCompanion[GenSeq]](cls)
        new SeqDeserializer[Any, GenSeq](c, t.containedType(0))
      } else if (classOf[SortedMap[_, _]].isAssignableFrom(cls)) {
        val c = companion[SortedMapFactory[SortedMap]](cls)
        val o = ordering(t.containedType(0))
        new SortedMapDeserializer[Any, Any](c, o, t.containedType(0), t.containedType(1))
      } else if (classOf[GenMap[_, _]].isAssignableFrom(cls)) {
        val c = companion[GenMapFactory[GenMap]](cls)
        new MapDeserializer[Any, Any](c, t.containedType(0), t.containedType(1))
      } else if (classOf[GenSet[_]].isAssignableFrom(cls)) {
        if (classOf[SortedSet[_]].isAssignableFrom(cls)) {
          val c = companion[SortedSetFactory[SortedSet]](cls)
          val o = ordering(t.containedType(0))
          new SortedSetDeserializer[Any, SortedSet](c, o, t.containedType(0))
        } else if (classOf[BitSet].isAssignableFrom(cls)) {
          val c = companion[BitSetFactory[BitSet]](cls)
          val t = cfg.getTypeFactory.constructType(classOf[Int])
          new BitSetDeserializer[BitSet](c, t)
        } else {
          val c = companion[GenericCompanion[GenSet]](cls)
          new SeqDeserializer[Any, GenSet](c, t.containedType(0))
        }
      } else if (classOf[mutable.PriorityQueue[_]].isAssignableFrom(cls)) {
        val c = companion[OrderedTraversableFactory[mutable.PriorityQueue]](cls)
        val o = ordering(t.containedType(0))
        new OrderedDeserializer[Any, mutable.PriorityQueue](c, o, t.containedType(0))
      } else {
        null
      }
    } else if (classOf[Option[_]].isAssignableFrom(cls)) {
      new OptionDeserializer(t.containedType(0))
    } else if (classOf[Product].isAssignableFrom(cls) && cls.getName.startsWith("scala.Tuple")) {
      new TupleDeserializer(t)
    } else if (classOf[Product].isAssignableFrom(cls)) {
      ScalaTypeSig(cfg.getTypeFactory, t) match {
        case Some(sts) if sts.isCaseClass =>
          new CaseClassDeserializer(sts.constructor, sts.annotatedAccessors)
        case _ =>
          null
      }
    } else if (classOf[Symbol].isAssignableFrom(cls)) {
      new SymbolDeserializer
    } else if (classOf[AnyRef].equals(cls)) {
      new FrontletUntypedObjectDeserializer(cfg)
    } else {
      null
    }
  }

  def companion[T](cls: Class[_]): T = {
    Class.forName(cls.getName + "$").getField("MODULE$").get(null).asInstanceOf[T]
  }

  lazy val orderings = Map[Class[_], Ordering[_]](
    classOf[Boolean]    -> Ordering.Boolean,
    classOf[Byte]       -> Ordering.Byte,
    classOf[Char]       -> Ordering.Char,
    classOf[Double]     -> Ordering.Double,
    classOf[Int]        -> Ordering.Int,
    classOf[Float]      -> Ordering.Float,
    classOf[Long]       -> Ordering.Long,
    classOf[Short]      -> Ordering.Short,
    classOf[String]     -> Ordering.String,
    classOf[BigInt]     -> Ordering.BigInt,
    classOf[BigDecimal] -> Ordering.BigDecimal)

  def ordering(t: JavaType): Ordering[Any] = {
    val cls = t.getRawClass
    orderings.getOrElse(cls, {
      val orderings = for (i <- 0 until t.containedTypeCount) yield ordering(t.containedType(0))
      val params = Array.fill(orderings.length)(classOf[Ordering[_]])
      val method = Ordering.getClass.getMethod(cls.getSimpleName, params:_*)
      method.invoke(Ordering, orderings:_*)
    }).asInstanceOf[Ordering[Any]]
  }
}

class FrontletUntypedObjectDeserializer(cfg: DeserializationConfig) extends BaseUntypedObjectDeserializer {
  val o = cfg.getTypeFactory.constructParametricType(classOf[collection.mutable.Map[_, _]], classOf[String], classOf[AnyRef])
  val a = cfg.getTypeFactory.constructParametricType(classOf[List[_]], classOf[AnyRef])

  override def mapArray(p: JsonParser, ctx: DeserializationContext): AnyRef = {
    val d = ctx.findContextualValueDeserializer(a, null)
    ctx.isEnabled(DeserializationFeature.USE_JAVA_ARRAY_FOR_JSON_ARRAY) match {
      case true  => mapArrayToArray(p, ctx)
      case false => d.deserialize(p, ctx)
    }
  }

  override def mapObject(p: JsonParser, ctx: DeserializationContext): AnyRef = {
    val d = ctx.findContextualValueDeserializer(o, null)
    d.deserialize(p, ctx)
  }
}


trait FrontletJacksonMapper {
  val mapper = new ObjectMapper
  mapper.registerModule(new FrontletJacksonModule)

  def readValue[T: Manifest](src: Array[Byte]): T = mapper.readValue(src, resolve)
  def readValue[T: Manifest](src: InputStream): T = mapper.readValue(src, resolve)
  def readValue[T: Manifest](src: Reader): T      = mapper.readValue(src, resolve)
  def readValue[T: Manifest](src: String): T      = mapper.readValue(src, resolve)
  def readValue[T: Manifest](src: JsonParser): T  = mapper.readValue(src, resolve)


  def readValues[T: Manifest](src: InputStream): MappingIterator[T] = mapper.readValues(mapper.getJsonFactory.createJsonParser(src), resolve)


  def writeValue(w: Writer, v: Any)         { mapper.writeValue(w, v) }
  def writeValue(o: OutputStream, v: Any)   { mapper.writeValue(o, v) }
  def writeValueAsString[T: Manifest](v: T) = writerWithType.writeValueAsString(v)

  def writerWithType[T: Manifest]           = mapper.writerWithType(resolve)

  val cache = JConcurrentMapWrapper(new ConcurrentHashMap[Manifest[_], JavaType])

  def resolve(implicit m: Manifest[_]): JavaType = cache.getOrElseUpdate(m, {
    def params = m.typeArguments.map(resolve(_))
    val tf = mapper.getTypeFactory
    m.typeArguments.isEmpty match {
      case true  => tf.constructType(m.erasure)
      case false => tf.constructParametricType(m.erasure, params: _*)
    }
  })
}

object FrontletJacksonMapper extends FrontletJacksonMapper
