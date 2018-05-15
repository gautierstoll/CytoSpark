
class FCSParserCompact(fcsNameInput: String, minValCytInput : Double) {

  import java.io._
  //import java.nio.file.{Files, Paths}
  //import scala.util.matching.Regex
  import breeze.linalg._
  import breeze.numerics._
  import java.nio.ByteBuffer
  import org.saddle._
  import stat._
  import scala.util._
  //import scala.math._
  import org.nspl.{DiscreteColors, TableLayout, point, sequence, xyplot,Plots}
  import org.nspl.saddle._
  import org.nspl.data._
  import org.nspl.awtrenderer._
  import org.saddle.io._
  import stat.kmeans.KMeansResult
  import stat.sparse.SMat

  private val offsetByteText : (Int,Int,Int) = (10,17,25)
  private val offsetByteAnalysis : (Int,Int,Int) = (42,49,57)

  val fcsFile = new String(fcsNameInput)
  val minValCyt = minValCytInput

  private val fcsFileBuffer = new BufferedInputStream(new FileInputStream(fcsFile))

  private var binaryFileIndex: Int = 0
  for (i <- (0 to (offsetByteText._1-1))) {
    fcsFileBuffer.read
    binaryFileIndex +=1
  }
  private val firstTextSegment = (for (i <- (offsetByteText._1 to offsetByteText._2)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex = offsetByteText._2+1

  private val lastTextSegment = (for (i <- (binaryFileIndex to offsetByteText._3)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteText._3+1
  for (i <- (binaryFileIndex to (offsetByteAnalysis._1-1))) yield {
    fcsFileBuffer.read
    binaryFileIndex +=1
  }
  private val firstAnalysisSegment = (for (i <- (binaryFileIndex to offsetByteAnalysis._2)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteAnalysis._2+1

  private val LastAnalysisSegment = (for (i <- (binaryFileIndex to offsetByteAnalysis._3)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex = offsetByteAnalysis._3+1;

  private def lengthSecondCharSep(inList: List[Byte]): Int = {
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

  private def textSegmentMap(inList: List[Byte]): Map[String, String] = {
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
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 32) {
      binaryFileIndex += 4
      val tempFileArray = ByteBuffer.wrap((1 to 4).map(x => fcsFileBuffer.read.toByte).toArray)
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex: Int = (0 to (compensatedParam.length - 1)).
          filter(x => (compensatedParam(x) == (1 + indexFCS - (indexFCS / nbPar) * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getFloat.toDouble-minValCyt)
      }
    }
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 64) {
      binaryFileIndex += 8
      val tempFileArray = ByteBuffer.wrap((1 to 8).map(x => fcsFileBuffer.read.toByte).toArray)
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex = (0 to (compensatedParam.length - 1)).
          filter(x => (compensatedParam(x) == (1 + indexFCS - (indexFCS / nbPar) * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getDouble-minValCyt)
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


  def kmeanCompensatedSubPlot(nbRowsFCS: Int, sizeK: Int, it: Int, seed: Int, maxComb : Int) = {
    val dataSubFCS = dataCompensatedMatFCS.row((0 to (nbRowsFCS - 1)).toArray)
    val rand4K = new Random(seed)
    val dataInitK = dataSubFCS.row((1 to sizeK).map(x => rand4K.nextInt(nbRowsFCS)).toArray)
    val resK = kmeans.apply(dataSubFCS, dataInitK, it)
    //val dataMatrixSubFCS = kmeans.matToSparse(dataSubFCS)
    val projections = 0 until maxComb combinations (2) map { g =>
      val c1 = g(0)
      val c2 = g(1)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      xyplot(
        Mat(col1, col2, resK.clusters.map(_.toDouble)) -> point(
          labelText = false,
          color = DiscreteColors(resK.clusters.length)))(
        extraLegend = resK.clusters.toArray.map(
          x =>
            x -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1),
              color = DiscreteColors(resK.clusters.length)(x.toDouble))),
        xlab = compensatedParam.map(x=> fcsTextSegmentMap("$P" + x+ "S")).toList(c1),
        ylab = compensatedParam.map(x=> fcsTextSegmentMap("$P" + x+ "S")).toList(c2)
      )
    }
    sequence(projections.toList, TableLayout(4))
  }
}
