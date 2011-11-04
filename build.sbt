seq(conscriptSettings :_*)

organization := "akihiro4chawon.github.com"

name := "gistyz"

version := "0.1.0-SNAPSHOT"

resolvers += "my repository" at "http://akihiro4chawon.github.com/gist4z/maven-repo/releases/"

libraryDependencies ++= Seq(
  "aki.gist4z" %% "gist4z" % "0.1",
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "net.liftweb" %% "lift-json-scalaz" % "2.4-M4",
  "net.liftweb" %% "lift-json-ext" % "2.4-M4",
  "net.liftweb" %% "lift-json" % "2.4-M4"
)

publishTo <<= (version) {
  version: String =>
    def repo(name: String) = Resolver.file("file", new File("../gistyz-gh-pages/maven-repo") / name)
    val isSnapshot = version.trim.endsWith("SNAPSHOT")
    val repoName   = if (isSnapshot) "snapshots" else "releases"
    Some(repo(repoName))
}

