//package org.rexo.crf
//
//import cc.factorie.app.chain.ChainModel
//import cc.factorie.variable.{LabeledCategoricalVariable, ChainLink, BinaryFeatureVectorVariable, CategoricalVectorDomain}
//import cc.factorie.{variable, app}
//import cc.factorie.app.nlp.segment.{BIOSegmentationDomain, SegmentationLabelDomain}
//import cc.factorie.app.nlp.{Document, Sentence, Token, DocumentAnnotator}
////import cc.factorie.app.chineseStrings._
//import java.io.File
//import scala.util.Random
//import cc.factorie.optimize.OnlineTrainer
//import edu.umass.cs.rexo.ghuang.segmentation.LineInfo
//import org.jdom2.input.SAXBuilder
//
///**
// * Created by klimzaporojets on 11/7/14.
// */
//
//object ChainReferenceSegmenter
//{
//  def main(args: Array[String]): Unit = {
//    println("running main")
//
//    val crs:ChainReferenceSegmenter = new ChainReferenceSegmenter()
//
////    val gto:scala.collection.GenTraversableOnce[String] = null ;
//
//    crs.train(List("/Users/klimzaporojets/klim/pdf2meta/pdf.extractor.js/svgdump/test5-"))
//  }
//}
//class ChainReferenceSegmenter(
//   labelDomain: ReferenceTag = ReferenceDomain //BIOSegmentationDomain //my own domain
// ) extends DocumentAnnotator
//{
//
//
//  //TODO: what does this do?
//  def process(document: Document): Document = {
//
////    //Since tokens are position-based, and the character tagger removes whitespace,
////    //its necessary to consider whitespace when creating tokens from tagged characters
////    val whiteSpaceOffsets = labelDomain.getWhiteSpaceOffsets(document.string)
////    val segmentedText = segment(document)
////
////    var tokenStart = 0
////
////    ( 0 to segmentedText.size ).foreach{ i =>
////
////      if( i == 0 || isEndOfSentence(segmentedText(i - 1).character.string(0)) )
////        new Sentence(document)
////
////      if( i > 0 && (i == segmentedText.size || labelDomain.indicatesSegmentStart(segmentedText(i).categoryValue))){
////        new Token(document, whiteSpaceOffsets(tokenStart) + tokenStart, whiteSpaceOffsets(i - 1) + i)
////
////        tokenStart = i
////      }
////    }
//
//    document
//  }
//
//
//  def populateFeatures(labeledCorpus: List[IndexedSeq[LineInfo]]): Unit = {
//    //TODO: here call LineInfo2TokenSequenceV2
//  }
//
//
//
//  def train(filePaths: List[String]):Unit = {
//    println("Training In Progress")
//    println("\tFeature Extraction In Progress")
//
//    val labeledCorpora:List[IndexedSeq[(LineInfo)]] = filePaths.map(
//
////      referenceLines:Seq[Any] = labelDomain.getReferenceLines()
//      filePath => labelDomain.getLabeledLines(/*new File(filePath)*/ filePath) //getLabeledCharacters(new File(filePath))
//    ) //.flatten.toIndexedSeq
//
//    //TODO: implement this one
//    populateFeatures(labeledCorpora)
//
//    val trainingSegmentables = getSegmentables(labeledCorpora)
//
//    ReferenceFeaturesDomain.freeze
//
//    println("\tFeature Extraction Completed")
//
//    val examples =
//      trainingSegmentables.map( segmentable =>
//        new model.ChainLikelihoodExample(segmentable.links.map( _.label ))
//      ).toList
//
//    Random.setSeed(0)
//
//    val shuffledExamples = Random.shuffle(examples)
//    val trainer = new OnlineTrainer(model.parameters)
//
//    trainer.trainFromExamples(shuffledExamples)
//
//    println("Training Complete\n")
//  }
//
////  def addFeatures(line:Line):Unit = {
////
////  }
//  object ReferenceFeaturesDomain extends CategoricalVectorDomain[String]
//  class ReferenceFeatures(val features: Seq[String])
//    extends BinaryFeatureVectorVariable[String] {
//
//    override def skipNonCategories = true
//    def domain = ReferenceFeaturesDomain
//
//    this ++= features
//  }
//
////  val token:Token;
//  class Line(character: String, labelString: String, featureSeq: Seq[String])
//    extends app.chain.Observation[Line]
//    with ChainLink[Line, Segmentable] {
//
//    val features = new ReferenceFeatures(featureSeq)
//    val label = new SegmentationLabel(labelString, this)
//
//    def string = character
//  }
//
//
//
//  class SegmentationLabel(labelName: String, val line: Line)
//    extends LabeledCategoricalVariable(labelName) {
//
//    def domain = labelDomain
//  }
//
//  //TODO: is it necessary to convert it in this way ?
//  def getSegmentables(labeledExamples: List[IndexedSeq[(LineInfo)]]
//      ): IndexedSeq[Segmentable] = {
//
//    val segmentables = labeledExamples.map(
//      example => new Segmentable ++= (0 until example.size).map(
//        i => new Line(example(i).text, example(i).label, example(i).presentFeatures.toSeq)
//      )
//    ).toIndexedSeq
//
//    println("Segmentables Retrieved")
//
//    segmentables
//  }
//
//  //TODO: why do I need this?
//  class Segmentable extends variable.Chain[Segmentable, Line]
//
//  val model = new ChainModel[SegmentationLabel, ReferenceFeatures, Line](
//    labelDomain,
//    ReferenceFeaturesDomain,
//    label => label.line.features,
//    label => label.line,
//    line => line.label
//  )
//
//  def postAttrs = Seq(classOf[Token], classOf[Sentence])
//
//
//  def prereqAttrs = Seq()
//
//
//  def tokenAnnotationString(token: Token): String = {
//    token.string + "\t"
//  }
//
//}
