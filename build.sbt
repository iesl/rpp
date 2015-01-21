organization := "edu.umass.cs.iesl"

name := "rpp"

version := "0.1-SNAPSHOT"
 
scalaVersion := "2.11.4"

resolvers ++= Seq(
  "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/public-snapshots",
  "IESL Public Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  Resolver.mavenLocal
)

libraryDependencies ++= Seq(
  "org.jdom" % "jdom2" % "2.0.5",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "cc.factorie" % "factorie_2.11" % "1.1",
  "dom4j" % "dom4j" % "1.6.1",
  "jaxen" % "jaxen" % "1.1.6",
  "edu.umass.cs.iesl.xml_annotator" % "xml_annotator" % "0.1-SNAPSHOT",
  "edu.umass.cs.iesl.paper_header" % "paper_header" % "0.1-SNAPSHOT",
  "edu.umass.cs.iesl.bibie" % "bibie" % "0.1-SNAPSHOT"
)
