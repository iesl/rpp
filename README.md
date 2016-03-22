# RPP #
Research Paper Processor

## Setup Prerequisites ##

### XML Annotator ###
Clone and install the project xml-annotator:

```
git clone https://github.com/iesl/xml-annotator
cd xml-annotator
mvn clean install
```

### Bibie ###

Clone and install the project bibie:

```
git clone https://github.com/iesl/bibie
cd bibie
mvn clean install
```

### Paper Header ###

Clone and install the project paper_header:

```
git clone https://github.com/iesl/paper-header
cd paper-header
mvn clean install
```


## Clone \& Build ##

```
git clone https://github.com/iesl/rpp
```

We need to build the project and jar the classes with the dependencies

```
mvn clean package -Pjar-with-dependencies
```

## Running the project ##

Download the pre-trained models:

```
./download.sh
```

Obtain the lexicon files. You can email me if you do not have these files.

See [this script](/batchrun.sh) for an example of running RPP. The syntax is:
```
./batchrun.sh /path/to/rpp/ /path/to/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar /path/to/input_dir/ /path/to/output_dir/
```

Note the input to RPP are svg files which can be obtained from the PDF using [iesl-pdf-to-text](https://github.com/iesl/iesl-pdf-to-text)

