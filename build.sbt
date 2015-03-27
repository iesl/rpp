organization := "edu.umass.cs.iesl"

name := "rpp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  //  "IESL repository" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases/",
  //  "IESL repository" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots/"
)

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies ++= Seq(
  "org.jdom" % "jdom2" % "2.0.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "cc.factorie" % "factorie_2.11" % "1.2-SNAPSHOT",
  "dom4j" % "dom4j" % "1.6.1",
  "jaxen" % "jaxen" % "1.1.6",
  "edu.umass.cs.iesl.xml_annotator" % "xml_annotator" % "0.1-SNAPSHOT",
  "edu.umass.cs.iesl.paper_header" % "paper_header" % "0.1-SNAPSHOT",
  "edu.umass.cs.iesl.bibie" % "bibie" % "0.1-SNAPSHOT"
)
