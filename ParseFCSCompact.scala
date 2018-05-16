
import java.io._
import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer
import org.saddle._
import stat._
import scala.util._
import org.nspl._
import org.nspl.saddle._
import org.nspl.data._
import org.nspl.awtrenderer._
import org.saddle.io._
import stat.kmeans._
import stat.sparse.SMat

class FCSParserCompact(fcsNameInput: String, minValCytInput: Double) {

  private val offsetByteText: (Int, Int, Int) = (10, 17, 25)
  private val offsetByteAnalysis: (Int, Int, Int) = (42, 49, 57)

  private def textSegmentMap(inList: List[Byte]): Map[String, String] = {
    def lengthSecondCharSep(inList: List[Byte]): Int = {
      def dropUntilSinglSep(ByteSeparator: Byte, offsetcharList: Int, charList: List[Byte]): Int = {
        var newOffsetcharList = offsetcharList + charList.drop(offsetcharList).takeWhile(_ != ByteSeparator).length
        charList.drop(newOffsetcharList) match {
          case ByteSeparator :: Nil => newOffsetcharList // two separators is not a separator
          case ByteSeparator :: ByteSeparator :: yy => dropUntilSinglSep(ByteSeparator, newOffsetcharList + 2, charList)
          case ByteSeparator :: yy => newOffsetcharList
          case yy => {
            sys.error("Error in Parsing text segment");
            0
          }
        }
      }

      dropUntilSinglSep(inList.head, 1, inList)
    }

    val keyLength = lengthSecondCharSep(inList) - 1
    val valLength = lengthSecondCharSep(inList.drop(keyLength + 1)) - 1
    if (inList.length <= (keyLength + valLength + 3)) {
      Map(inList.drop(1).take(keyLength).map(_.toChar).mkString("") ->
        inList.drop(1 + keyLength + 1).take(valLength).map(_.toChar).mkString(""))
    }
    else {
      Map(inList.drop(1).take(keyLength).map(_.toChar).mkString("") ->
        inList.drop(1 + keyLength + 1).take(valLength).map(_.toChar).mkString("")) ++
        textSegmentMap(inList.drop(1 + keyLength + 1 + valLength))
    }
  }


  val fcsFile = new String(fcsNameInput)
  val minValCyt = minValCytInput

  private val fcsFileBuffer = new BufferedInputStream(new FileInputStream(fcsFile))

  private var binaryFileIndex: Int = 0
  for (i <- (0 to (offsetByteText._1 - 1))) {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstTextSegment = (for (i <- (offsetByteText._1 to offsetByteText._2)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex = offsetByteText._2 + 1

  private val lastTextSegment = (for (i <- (binaryFileIndex to offsetByteText._3)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteText._3 + 1
  for (i <- (binaryFileIndex to (offsetByteAnalysis._1 - 1))) yield {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstAnalysisSegment = (for (i <- (binaryFileIndex to offsetByteAnalysis._2)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteAnalysis._2 + 1

  private val LastAnalysisSegment = (for (i <- (binaryFileIndex to offsetByteAnalysis._3)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex = offsetByteAnalysis._3 + 1;

  for (i <- (binaryFileIndex to (firstTextSegment - 1))) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = firstTextSegment;

  private val fcsTextSegment = (for (i <- (binaryFileIndex to lastTextSegment)) yield {
    fcsFileBuffer.read
  }).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1;
  val fcsTextSegmentMap = textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Byte order: " + fcsTextSegmentMap("$BYTEORD"))
  println("Number of events: " + fcsTextSegmentMap("$TOT"))

  private val firstDataSegment = fcsTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  private val lastDataSegment = fcsTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  val nbPar = fcsTextSegmentMap("$PAR").toInt
  val nbEvent = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  val bitToFloat = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList
  val compensatedParam = (1 to bitToFloat.length).filter(x => fcsTextSegmentMap.contains("$P" + x + "S"))
  compensatedParam.map(x => println("$P" + x + "S -> " + fcsTextSegmentMap("$P" + x + "S")))

  for (i <- (binaryFileIndex to (firstDataSegment - 1))) yield {
    fcsFileBuffer.read
  }
  binaryFileIndex = firstDataSegment

  private var dataCompensatedArrayFCS = new Array[Double](nbEvent * compensatedParam.length)
  for (indexFCS <- (0 to (nbEvent * nbPar - 1))) {
    print(indexFCS + "\r")
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 32) {
      binaryFileIndex += 4
      val tempFileArray = ByteBuffer.wrap((1 to 4).map(x => fcsFileBuffer.read.toByte).toArray)
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex: Int = (0 to (compensatedParam.length - 1)).
          filter(x => (compensatedParam(x) == (1 + indexFCS - (indexFCS / nbPar) * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getFloat.toDouble - minValCyt)
      }
    }
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 64) {
      binaryFileIndex += 8
      val tempFileArray = ByteBuffer.wrap((1 to 8).map(x => fcsFileBuffer.read.toByte).toArray)
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex = (0 to (compensatedParam.length - 1)).
          filter(x => (compensatedParam(x) == (1 + indexFCS - (indexFCS / nbPar) * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getDouble - minValCyt)
      }
    }
  }
  val dataCompensatedMatFCS: Mat[Double] = Mat(nbEvent, compensatedParam.length, dataCompensatedArrayFCS)

  def getCompensatedMatrixFCS: DenseMatrix[Double] = {
    var FCSMatrix = new DenseMatrix[Double](nbEvent, compensatedParam.length)
    for (rowFCS <- (0 to (nbEvent - 1)); colFCS <- (0 to (compensatedParam.length - 1))) {
      FCSMatrix(rowFCS, colFCS) = dataCompensatedArrayFCS(colFCS + rowFCS * compensatedParam.length)
    }
    return (FCSMatrix)
  }

  def kmeansCompensated(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    val dataSubFCS = dataCompensatedMatFCS.row((0 to (kMeanFCSInput.nbRows - 1)).toArray)
    val rand4K = new Random(kMeanFCSInput.seedK)
    val dataInitK = dataSubFCS.row((1 to kMeanFCSInput.clusterNb).
      map(x => rand4K.nextInt(kMeanFCSInput.nbRows)).toArray)
    kmeans.apply(dataSubFCS, dataInitK, kMeanFCSInput.iterations)
  }
}

case class KMeanFCSInput(clusterNb: Int = 5, nbRows: Int = 100, iterations: Int = 100, seedK: Int = 0) {}

object FCSPlotting {
  def kMeanFCSPlotSeq(fcsParsed: FCSParserCompact, kMeanR: KMeansResult, exludeCluster : Array[Int] = Array())
 : Build[ElemList[Elems2[XYPlotArea,Legend]]] = {
    val keepIndex = (0 to (kMeanR.clusters.length - 1)).
      filter(x => (! exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataCompensatedMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(exludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = 0 until fcsParsed.compensatedParam.length combinations(2) map { g =>
      val c1 = g(0)
      val c2 = g(1)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      xyplot(
        Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)) -> point(
          labelText = false, size = 4.0/log10(kMeanR.clusters.length),
          color = DiscreteColors(kMeanR.clusters.length)))(
        extraLegend = subKMeanR.clusters.toArray.distinct.map(
          x =>
            x.toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1),
              color = DiscreteColors(kMeanR.clusters.length)(x.toDouble))),
        xlab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c1),
        ylab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c2)
      )
    }
    sequence(projections.toList, TableLayout(4))
  }
  def plotKSeqToPng(plotSeq : Build[ElemList[Elems2[org.nspl.XYPlotArea,org.nspl.Legend]]],
                    fileName : String, widthPng : Int = 1000) = {
    val filePng = new File(fileName)
    pngToFile(filePng,plotSeq.build,widthPng)
  }
}