# RPP #
Research Paper Processor

This branch of RPP outputs paragraph markers. Currently under development.

## Setup Prerequisites ##

### Bibie ###

Clone and install the project bibie:

```
git clone https://github.com/iesl/bibie
cd bibie
mvn install
```

### Paper Header ###

Clone and install the project paper_header:

```
git clone https://github.com/iesl/paper-header
cd paper-header
mvn install
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

Unzip the models in the rpp repo:

```
tar -xvf models.tgz
```

Obtain the lexicon files. You can email me if you do not have these files.

See [this script](/batchrun.sh) for an example of running RPP.

Note the input to RPP are svg files which can be obtained from the PDF using [iesl-pdf-to-text](https://github.com/iesl/iesl-pdf-to-text)

