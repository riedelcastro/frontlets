import org.riedelcastro.frontlets.FrontletJacksonMapper

def getCCParams(cc: AnyRef) =
  (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
    f.setAccessible(true)
    a + (f.getName -> f.get(cc))
  }

case class A(i:Int)
case class B(a:A)
val b = B(A(4))
val json = FrontletJacksonMapper.writeValueAsString(b)
val parsed = FrontletJacksonMapper.readValue[B](json)


println(json)

