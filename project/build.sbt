// This resolver declaration is added by default in SBT 0.12.x
resolvers += Resolver.url(
  "sbt-plugin-releases", 
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.5")

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

resolvers += "sonatype-releases" at "https://oss.sonatype.org/service/local/repositories/releases/content/"

addSbtPlugin("com.jsuereth" % "sbt-site-plugin" % "0.5.0")

