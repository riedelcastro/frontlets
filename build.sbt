name := "frontlets"

organization := "org.riedelcastro.frontlets"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
//version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.9.1", "2.9.2","2.10.0", "2.10.2")

scalaVersion := "2.9.2"

resolvers ++= Seq(
    "IESL third party" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/",
    "IESL snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots",
    "IESL releases" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases"
)


scalacOptions ++= Seq("-unchecked","-deprecation")

// Add multiple dependencies
libraryDependencies ++= Seq(
     "com.lambdaworks" % "jacks" % "2.0.4",
     "org.mongodb" % "mongo-java-driver" % "2.10.1",
     "org.riedelcastro.nurupo" %% "nurupo" % "0.1-SNAPSHOT",
     "org.scalatest" %% "scalatest" % "1.9.1" % "test",
     "org.mockito" % "mockito-all" % "1.9.0" % "test",
     "org.apache.bcel" % "bcel" % "5.2"
//     "com.novus" %% "salat" % "1.9.1"
)

publishTo <<= (version) { version: String =>
  val homeniscient = "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at homeniscient + "snapshots/")
  else                                   Some("releases"  at homeniscient + "releases/")
}


credentials += Credentials(Path.userHome / ".ivy2" / ".credentials-homeniscient")

releaseSettings

site.settings

site.includeScaladoc()

//seq(site.settings:_*)

seq(ghpages.settings:_*)

git.remoteRepo := "git@github.com:riedelcastro/frontlets.git"

//fork in run := true

//javaOptions in run += "-Xmx8G"

