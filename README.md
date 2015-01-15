RPP
===

Research Paper Processor

Data files
----------

RPP depends on libraries that need access to FACTORIE's lexicon files. To provide these to the program, it is currently necessary to provide a url (resolved by `java.net.URL`) using a java system property. If your lexicon file is located at `/Users/bob/data/lexicons`, you would provide

```bash
  java -Dcc.factorie.app.nlp.lexicon.Lexicon="file:///Users/bob/data/lexicons"
```

to your java command.
