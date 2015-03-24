RPP
===

Research Paper Processor

Build and run Main from command line
------------------------------------
```bash
  run.sh file:///lexicons/uri file:///reference/crf/model/uri header/crf/model/path \
    input/pdf/file/path output/svg/file/path
```

Manage with SBT
----------------
```scala
  libraryDependencies += "edu.umass.cs.iesl.rpp" % "rpp" % "0.1-SNAPSHOT"
```

Use from Scala 
--------------
```scala
  import org.jdom2.input.SAXBuilder
  val builder = new SAXBuilder()
  val dom = builder.build(new File(/*input pdf path*/)) 

  val l = List(
      LineProcessor, 
      StructureProcessor, 
      ReferencePartProcessor("file:///your/reference/crf/model/uri"), 
      CitationProcessor, 
      CitationReferenceLinkProcessor, 
      HeaderPartProcessor("your/header/crf/model") /* 
      * Depends on FACTORIE's lexicon files.
      * The files can be loaded from cli when you run your java/scala program: 
      * java -Dcc.factorie.app.nlp.lexicon.Lexicon="file:///your/lexicons/uri"
      * or
      * sbt -Dcc.factorie.app.nlp.lexicon.Lexicon="file:///your/lexicons/uri"
      */
  )

  val annotator = l.foldLeft(Annotator(dom)) {
    case (annoAcc, pro) => pro.process(annoAcc)
  } 
```

See Main.scala for more details
