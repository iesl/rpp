organization := "edu.umass.cs.iesl"

name := "rpp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += Resolver.mavenLocal

// "org.jdom" % "jdom2" % "2.0.6",
// "dom4j" % "dom4j" % "1.6.1",
// "jaxen" % "jaxen" % "1.1.6",

libraryDependencies ++= Seq(
  "edu.umass.cs.iesl.bibie" % "bibie" % "1.0-SNAPSHOT",
  "edu.umass.cs.iesl.paper_header" % "paper_header" % "0.1-SNAPSHOT",
  "edu.umass.cs.iesl.xml_annotator" % "xml_annotator" % "0.1-SNAPSHOT",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.scalatest" %% "scalatest" % "2.2.1",
  "cc.factorie" %% "factorie" % "1.2-SNAPSHOT",
  "cc.factorie.app.nlp" % "all-models" % "1.2-SNAPSHOT"
)

parallelExecution := true
