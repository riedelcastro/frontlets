name := "frontlets"

organization := "org.riedelcastro.frontlets"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
//version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.9.1", "2.9.2")

scalaVersion := "2.9.2"

resolvers ++= Seq(
    "conjars.org" at "http://conjars.org/repo",
    "IESL third party" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/",
    "IESL snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots",
    "IESL releases" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases"
)


scalacOptions ++= Seq("-unchecked","-deprecation")

// Add multiple dependencies
libraryDependencies ++= Seq( 
     "org.mongodb" % "mongo-java-driver" % "2.7.3", 
     "org.riedelcastro.nurupo" %% "nurupo" % "0.1-SNAPSHOT",
     "org.scalatest" %% "scalatest" % "1.8" % "test"
)

publishTo <<= (version) { version: String =>
  val iesl = "https://dev-iesl.cs.umass.edu/nexus/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at iesl + "snapshots/")
  else                                   Some("releases"  at iesl + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

releaseSettings

//fork in run := true

//javaOptions in run += "-Xmx8G"
