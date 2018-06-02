
import java.io._
import breeze.linalg._
import breeze.numerics._
import breeze.stats._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.{Paths, Files}
import org.saddle._
import stat._
import scala.util._
import breeze.numerics.constants._
import org.nspl._
import org.nspl.saddle._
import org.nspl.data._
import org.nspl.awtrenderer._
import org.saddle.io._
import stat.kmeans._
import stat.sparse.SMat
import stat.sparse.SVec
import scala.collection.parallel.mutable._
import org.saddle.io.CsvImplicits._

//Companion object
object FCSParserFull {
  val offsetByteText: (Int, Int, Int) = (10, 17, 25)
  val offsetByteAnalysis: (Int, Int, Int) = (42, 49, 57)
  val defaultMinLog = 0.0

  // method for reading paramters map
  def textSegmentMap(inList: List[Byte]): Map[String, String] = {
    def lengthSecondCharSep(inList: List[Byte]): Int = {
      def dropUntilSinglSep(ByteSeparator: Byte, offsetcharList: Int, charList: List[Byte]): Int = {
        val newOffsetcharList = offsetcharList + charList.drop(offsetcharList).takeWhile(_ != ByteSeparator).length
        charList.drop(newOffsetcharList) match {
          case ByteSeparator :: Nil => newOffsetcharList // two separators is not a separator
          case ByteSeparator :: ByteSeparator :: yy => dropUntilSinglSep(ByteSeparator, newOffsetcharList + 2, charList)
          case ByteSeparator :: yy => newOffsetcharList
          case Nil => (newOffsetcharList - 1)
          case yy => {
            sys.error("Error in Parsing text segment: " + yy)
            0
          }
        }
      }

      dropUntilSinglSep(inList.head, 1, inList)
    }

    val keyLength = lengthSecondCharSep(inList) - 1
    val valLength = lengthSecondCharSep(inList.drop(keyLength + 1)) - 1
    if (inList.length <= (keyLength + valLength + 3))
      Map(inList.slice(1, 1 + keyLength).map(_.toChar).mkString("") ->
        inList.slice(1 + keyLength + 1, 1 + keyLength + 1 + valLength).map(_.toChar).mkString("")) else
      Map(inList.slice(1, 1 + keyLength).map(_.toChar).mkString("") ->
        inList.slice(1 + keyLength + 1, 1 + keyLength + 1 + valLength).map(_.toChar).mkString("")) ++
        textSegmentMap(inList.drop(1 + keyLength + 1 + valLength))
  }
}

// class for reading header of fcs
class FCSHeader(fcsNameInput: String) {
  val fcsFile = new String(fcsNameInput)
  if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " not found")
  private val fcsFileBuffer =
    new BufferedInputStream(new FileInputStream(fcsFile))
  private var binaryFileIndex: Int = 0
  for (i <- 0 until FCSParserFull.offsetByteText._1) {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstTextSegment =
    (for (i <- FCSParserFull.offsetByteText._1 to FCSParserFull.offsetByteText._2) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteText._2 + 1

  private val lastTextSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteText._3) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteText._3 + 1
  for (i <- binaryFileIndex until FCSParserFull.offsetByteAnalysis._1) {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstAnalysisSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteAnalysis._2) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteAnalysis._2 + 1

  private val lastAnalysisSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteAnalysis._3) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteAnalysis._3 + 1

  for (i <- binaryFileIndex until firstTextSegment) fcsFileBuffer.read
  binaryFileIndex = firstTextSegment
  // reading text segment
  private val fcsTextSegment =
    (for (i <- binaryFileIndex to lastTextSegment) yield fcsFileBuffer.read).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1
  val fcsTextSegmentMap: Map[String, String] = FCSParserFull.textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE") +
    (if (fcsTextSegmentMap("$MODE") == "L") " OK" else " can't get data"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE") +
    (if ((fcsTextSegmentMap("$DATATYPE") == ("F")) || (fcsTextSegmentMap("$DATATYPE") == "D")) " OK"
    else " can't get data"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Byte order: " + fcsTextSegmentMap("$BYTEORD"))
  println("Number of events: " + fcsTextSegmentMap("$TOT"))
  val nbPar: Int = fcsTextSegmentMap("$PAR").toInt
  val nbEvent: Int = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  fcsFileBuffer.close()
  println("Size of data: " + nbEvent * nbPar)

  // method for constructin FCSInputFull from prompt
  def getOnlineFCSInput: FCSInputFull = {
    val tParam = (1 to nbPar).map(param => {
      val paramName = fcsTextSegmentMap("$P" + param + "N") +
        (if (fcsTextSegmentMap.contains("$P" + param + "S")) " -> " + fcsTextSegmentMap("$P" + param + "S") else "")
      println()
      print(paramName)
      val boolTake = !(scala.io.StdIn.readLine(", Take [y]/n? ") == "n")
      val boolLog = if (boolTake) !(scala.io.StdIn.readLine("Log [y]/n? ") == "n") else false
      val minVal = if (boolLog) {
        scala.io.StdIn.readLine("Minimum value [" + FCSParserFull.defaultMinLog + "]: ") match {
          case "" => FCSParserFull.defaultMinLog
          case resp: String => try {
            resp.toDouble
          } catch {
            case _: Throwable => {
              println("Take " + FCSParserFull.defaultMinLog);
              FCSParserFull.defaultMinLog
            }
          }
        }
      }
      else {
        FCSParserFull.defaultMinLog
      }
      (boolTake, param, boolLog, minVal)
    }).filter(x => x._1).map(x => (x._2, x._3, x._4))
    val tNbEvent = scala.io.StdIn.readLine("Nb of events [" + nbEvent + "]: ") match {
      case "" => nbEvent
      case res: String => try {
        res.toInt
      } catch {
        case _: Throwable => {
          println("Take " + nbEvent);
          nbEvent
        }
      }
    }
    FCSInputFull(fcsFile, tParam.toList, tNbEvent)
  }
}

// structure for FCSParserFull, takeParameters are indices (start at 1), log ? , min for Log
case class FCSInputFull(file: String, takeParameter: List[(Int, Boolean, Double)], takeNbEvent: Int) {}

// Full class for parsing fcs file and clustering (kmean, kmean++, test of convergence)
class FCSParserFull(fcsInput: FCSInputFull) {
  val fcsFile = new String(fcsInput.file)
  if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " not found")
  private val fcsFileBuffer = new BufferedInputStream(new FileInputStream(fcsFile))
  private var binaryFileIndex: Int = 0
  for (i <- 0 until FCSParserFull.offsetByteText._1) {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstTextSegment =
    (for (i <- FCSParserFull.offsetByteText._1 to FCSParserFull.offsetByteText._2) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteText._2 + 1
  private val lastTextSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteText._3) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteText._3 + 1
  for (i <- binaryFileIndex until FCSParserFull.offsetByteAnalysis._1) yield {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstAnalysisSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteAnalysis._2) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteAnalysis._2 + 1
  private val lastAnalysisSegment =
    (for (i <- binaryFileIndex to FCSParserFull.offsetByteAnalysis._3) yield fcsFileBuffer.read).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = FCSParserFull.offsetByteAnalysis._3 + 1
  for (i <- binaryFileIndex until firstTextSegment) fcsFileBuffer.read
  binaryFileIndex = firstTextSegment
  // contruct text segment map
  private val fcsTextSegment =
    (for (i <- binaryFileIndex to lastTextSegment) yield fcsFileBuffer.read).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1
  val fcsTextSegmentMap: Map[String, String] = FCSParserFull.textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Number of events: " + fcsTextSegmentMap("$TOT"))
  private val firstDataSegment = fcsTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  private val lastDataSegment = fcsTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  val nbPar: Int = fcsTextSegmentMap("$PAR").toInt
  val nbEvent: Int = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  val bitToFloat: List[Int] = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList
  val takenParam: scala.collection.immutable.IndexedSeq[Int] = fcsInput.takeParameter.map(_._1).toIndexedSeq

  for (i <- binaryFileIndex until firstDataSegment) yield fcsFileBuffer.read
  binaryFileIndex = firstDataSegment
  println("Size of data: " + fcsInput.takeNbEvent * nbPar)
  // construct data matrix
  private val dataTakenArrayFCS =
    new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  for (indexFCS <- 0 until fcsInput.takeNbEvent * nbPar) {
    print(indexFCS + "\r")
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 32) {
      binaryFileIndex += 4
      var tempFileArray = ByteBuffer.wrap((1 to 4).map(x => fcsFileBuffer.read.toByte).toArray)
      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
      }
      if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val takenIndex = takenParam.indices.
          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
        val readValue = tempFileArray.getFloat.toDouble
        val takenArrayIndex: Int = takenParam.indices.
          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
          (indexFCS / nbPar) * takenParam.length
        dataTakenArrayFCS(takenArrayIndex) = readValue
      }
    }
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 64) {
      binaryFileIndex += 8
      val tempFileArray = ByteBuffer.wrap((1 to 8).map(x => fcsFileBuffer.read.toByte).toArray)
      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
      }
      if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val takenIndex = takenParam.indices.
          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
        val readValue = tempFileArray.getDouble
        val takenArrayIndex = takenParam.indices.
          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
          (indexFCS / nbPar) * takenParam.length
        dataTakenArrayFCS(takenArrayIndex) = readValue
      }
    }
  }
  private val minColTakenMap = dataTakenArrayFCS.zipWithIndex.
    groupBy(x => (x._2 - x._2 / takenParam.length * takenParam.length)).map(x => (x._1, x._2.map(_._1).min))
  println("Apply logs") // take log, according to input
  for (indexFCS <- 0 until fcsInput.takeNbEvent * nbPar) { // could make a faster loop...
    print(indexFCS + "\r")
    if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
      val takenIndex = takenParam.indices.
        filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
      val takenArrayIndex: Int = takenParam.indices.
        filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
        (indexFCS / nbPar) * takenParam.length
      if (fcsInput.takeParameter(takenIndex)._2) {
        dataTakenArrayFCS(takenArrayIndex) =
          log10(dataTakenArrayFCS(takenArrayIndex) - minColTakenMap(takenIndex) +
            pow(10, fcsInput.takeParameter(takenIndex)._3))
      }
    }
  }
  private val dataNormalizedTakenArrayFCS =
    new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  // mean and meanSquare are not private, because they are use for unormalize data plotting (means from kmeans)
  val meanColTakenMap = dataTakenArrayFCS.zipWithIndex.
    groupBy(x => (x._2 - x._2 / takenParam.length * takenParam.length)).
    map(x => (x._1, x._2.map(_._1).sum / (fcsInput.takeNbEvent)))
  val meanSquareColTakenMap = dataTakenArrayFCS.zipWithIndex.
    groupBy(x => (x._2 - x._2 / takenParam.length * takenParam.length)).
    map(x => (x._1, (x._2.map(x => (x._1) * (x._1)).sum / (fcsInput.takeNbEvent))))
  println("Normalize") //Normalize data for clustering
  for (indexFCS <- 0 until fcsInput.takeNbEvent * nbPar) { // could make a faster loop...
    print(indexFCS + "\r")
    if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
      val takenIndex = takenParam.indices.
        filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
      val takenArrayIndex: Int = takenParam.indices.
        filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
        (indexFCS / nbPar) * takenParam.length
      dataNormalizedTakenArrayFCS(takenArrayIndex) = (dataTakenArrayFCS(takenArrayIndex) - meanColTakenMap(takenIndex)) /
        pow(meanSquareColTakenMap(takenIndex) - meanColTakenMap(takenIndex) * meanColTakenMap(takenIndex), .5)
    }
  }
  // construct saddle matrices of data
  val dataTakenMatFCS: Mat[Double] = Mat(fcsInput.takeNbEvent, takenParam.length, dataTakenArrayFCS)
  val dataNormalizedTakenMatFCS: Mat[Double] = Mat(fcsInput.takeNbEvent, takenParam.length, dataNormalizedTakenArrayFCS)

  def getCompensatedMatrixFCS: DenseMatrix[Double] = {
    val FCSMatrix = new DenseMatrix[Double](fcsInput.takeNbEvent, takenParam.length)
    for (rowFCS <- (0 until fcsInput.takeNbEvent); colFCS <- takenParam.indices) {
      FCSMatrix(rowFCS, colFCS) = dataTakenArrayFCS(colFCS + rowFCS * takenParam.length)
    }
    FCSMatrix
  }

  // kmean clustering
  def kmeansFCS(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    val dataSubFCS = dataNormalizedTakenMatFCS.row((0 until kMeanFCSInput.nbRows).toArray)
    val rand4K = new Random(kMeanFCSInput.seedK)
    val dataInitK = dataSubFCS.row((1 to kMeanFCSInput.clusterNb).
      map(x => rand4K.nextInt(kMeanFCSInput.nbRows)).toArray)
    kmeans.apply(dataSubFCS, dataInitK, kMeanFCSInput.iterations)
  }

  // kmean++ clustering
  def kmeansPPFCS(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    def initClust(initClustListIndex: List[Int], dataArrayIndex: Array[Int], clusterNb: Int, rand4Init: Random):
    List[Int] = {
      if (clusterNb == 0) initClustListIndex else {
        val dataIndexWithMinEuclid = dataArrayIndex.zip(
          dataArrayIndex.map(dataIndex =>
            initClustListIndex.map(initClustIndex =>
              kmeans.euclid(SVec(Series(dataNormalizedTakenMatFCS.row(initClustIndex)), dataNormalizedTakenMatFCS.numCols),
                dataNormalizedTakenMatFCS.row(dataIndex))).min))
        val random4Index = rand4Init.nextDouble() * dataIndexWithMinEuclid.map(_._2).sum
        val indexData4Clust = dataIndexWithMinEuclid.scanLeft(0, 0d)((x, y) => (y._1, x._2 + y._2)).
          filter(x => x._2 > random4Index).head._1
        initClust(indexData4Clust :: initClustListIndex,
          dataArrayIndex.filter(x => x != indexData4Clust), clusterNb - 1, rand4Init)
      }
    }

    val rand4K = new Random(kMeanFCSInput.seedK)
    val initDataIndex = rand4K.nextInt(kMeanFCSInput.nbRows)
    val clusterIndices = initClust(List(initDataIndex),
      (0 until kMeanFCSInput.nbRows).filter(x => x != initDataIndex).toArray,
      kMeanFCSInput.clusterNb - 1, rand4K).toArray
    kmeans.apply(dataNormalizedTakenMatFCS.row((0 until kMeanFCSInput.nbRows).toArray),
      dataNormalizedTakenMatFCS.row(clusterIndices), kMeanFCSInput.iterations)
  }

  // kmean clustering, several steps and several attemps (parralelized), with euclid norm quality
  def kmeansFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // carefull: it correspond to iterations*stepK + (stepk -1) or something like that
    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], nbRows: Int, iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) Nil else {
        print("Step " + step + "\r")
        val stepKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row((0 until nbRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataNormalizedTakenMatFCS.row((0 until nbRows).toArray))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / nbRows / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, nbRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val dataInitK = dataNormalizedTakenMatFCS.row((0 until kMeanFCSInput.nbRows).toArray).
        row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.nbRows)).toArray)
      val listEuclidRand = listEuclid(dataInitK.rows, kMeanFCSInput.nbRows, kMeanFCSInput.iterations, stepK)
      print("Finish seed " + seedKFromArray + "\r")
      (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
    })
  }

  // kmean++ clustering, several steps and several attemps (parralelized), with euclid norm quality
  def kmeansPPFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // carefull: it correspond to iterations*stepK + (stepk -1) or something like that
    def initClust(initClustListIndex: List[Int], dataArrayIndex: Array[Int], clusterNb: Int, rand4Init: Random):
    List[Int] = {
      if (clusterNb == 0) initClustListIndex else {
        val dataIndexWithMinEuclid = dataArrayIndex.zip(
          dataArrayIndex.map(dataIndex =>
            initClustListIndex.map(initClustIndex =>
              kmeans.euclid(SVec(Series(dataNormalizedTakenMatFCS.row(initClustIndex)), dataNormalizedTakenMatFCS.numCols),
                dataNormalizedTakenMatFCS.row(dataIndex))).min))
        val random4Index = rand4Init.nextDouble() * dataIndexWithMinEuclid.map(_._2).sum
        val indexData4Clust = dataIndexWithMinEuclid.scanLeft(0, 0d)((x, y) => (y._1, x._2 + y._2)).
          filter(x => x._2 > random4Index).head._1
        initClust(indexData4Clust :: initClustListIndex,
          dataArrayIndex.filter(x => x != indexData4Clust), clusterNb - 1, rand4Init)
      }
    }

    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], nbRows: Int, iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) Nil else {
        println("Step " + step)
        val stepKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row((0 until nbRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataNormalizedTakenMatFCS.row((0 until nbRows).toArray))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / nbRows / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, nbRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val initDataIndex = rand4K.nextInt(kMeanFCSInput.nbRows)
      val clusterIndices = initClust(List(initDataIndex),
        (0 until kMeanFCSInput.nbRows).filter(x => x != initDataIndex).toArray,
        kMeanFCSInput.clusterNb - 1, rand4K).toArray
      val listEuclidRand = listEuclid(dataNormalizedTakenMatFCS.row(clusterIndices).rows,
        kMeanFCSInput.nbRows, kMeanFCSInput.iterations, stepK)
      (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
    })
  }

  // kmean clustering, several steps and several attemps
  def kmeansFCSTestConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: Array[Int]):
  Array[List[IndexedSeq[Vec[Double]]]] = {
    def listMeanKMeans(initKMeans: IndexedSeq[Vec[Double]], nbRows: Int, iterations: Int, step: Int)
    : List[IndexedSeq[Vec[Double]]] = {
      if (step == 0) Nil else {
        val meanKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row((0 until nbRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations).means
        meanKMeans :: listMeanKMeans(meanKMeans, nbRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val dataInitK = dataNormalizedTakenMatFCS.row((0 until kMeanFCSInput.nbRows).toArray).
        row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.nbRows)).toArray)
      listMeanKMeans(dataInitK.rows, kMeanFCSInput.nbRows, kMeanFCSInput.iterations, stepK)
    })
  }
}

// structure for kmean of FCS
case class KMeanFCSInput(clusterNb: Int = 5, nbRows: Int = 100, iterations: Int = 100, seedK: Int = 0) {}

// methods for output
object FCSOutput {
  // 2D scatter plots
  def kMeanFCSPlot2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, exludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(exludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = fcsParsed.takenParam.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "        \r")
      val xMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c1).min, dataSubFCS.columnMinMax(c1).max)
      val yMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c2).min, dataSubFCS.columnMinMax(c2).max)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      xyplot(
        Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)) -> point(
          labelText = false, size = 4.0 / log10(kMeanR.clusters.length),
          color = DiscreteColors(kMeanR.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(kMeanR.means.length - 1)(x.toDouble))),
        xlab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "N")
        },
        ylab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  //2d ellipse plots

  def kMeanFCSPlotEllipse2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, exludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    def ellipsePoints2DVar(xData: Array[Double], yData: Array[Double], segmentNb: Int): org.saddle.Mat[Double] = {
      def ellipse2DVarCurve(data1: Array[Double], data2: Array[Double]): (Double => (Double, Double)) = {
        val svdCov = svd(covmat(DenseMatrix(data1, data2).t))
        val Vt = svdCov.Vt
        val S = svdCov.S
        (angle => {
          val vectEllipse = Vt * DenseVector(pow(S(0), .5) * cos(angle), pow(S(1), .5) * sin(angle))
          (vectEllipse(0) + breeze.stats.mean(data1), vectEllipse(1) + breeze.stats.mean(data2))
        })
      }

      val ellipseD1D2 = ellipse2DVarCurve(xData, yData)
      Mat(segmentNb + 1, 2, (0 to (segmentNb)).map(x => x.toDouble / segmentNb * 2 * Pi).map(x => ellipseD1D2(x)).flatMap(x => Array(x._1, x._2)).toArray)
    }

    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(exludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = fcsParsed.takenParam.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "     \r")
      val xMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c1).min, dataSubFCS.columnMinMax(c1).max)
      val yMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c2).min, dataSubFCS.columnMinMax(c2).max)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      // full matrix, with x and y points for ellipse, with color
      val clusterEllipseMatForPlot = Mat(subKMeanR.clusters.toSeq.distinct.flatMap(clusterIndex =>
        (ellipsePoints2DVar(col1.toSeq.zip(subKMeanR.clusters.toSeq).filter(x => (x._2 == clusterIndex)).map(x => x._1).toArray,
          col2.toSeq.zip(subKMeanR.clusters.toSeq).filter(x => (x._2 == clusterIndex)).map(x => x._1).toArray,100).cols.toList :::
          List(Vec(Array.fill(100+1)(clusterIndex.toDouble))))).toArray)
      xyplot(clusterEllipseMatForPlot ->
        (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => line(xCol = x * 3, yCol = x * 3 + 1, colorCol = x * 3 + 2,
          color = DiscreteColors(kMeanR.means.length - 1))).toList)(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(kMeanR.means.length - 1)(x.toDouble))),
        xlab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "N")
        },
        ylab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))
  }


  // 2d plots of kmean cluster centers
  def kMeanFCSPlotClusters2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, exludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val clusterSize = kMeanR.clusters.toArray.groupBy(x => x).map(x => (x._1, x._2.length)).
      filter(x => (!exludeCluster.contains(x._2))).toList.sortBy(_._1)
    val clusterMean = kMeanR.means.zipWithIndex.filter(x => (!exludeCluster.contains(x._2)))
    val projections = kMeanR.means.head.toArray.indices.combinations(2).map { g =>
      val c1 = (g(0))
      val c2 = (g(1))
      print(c1 + " x " + c2 + "       " + "\r")
      val xMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c1).min, dataSubFCS.columnMinMax(c1).max)
      val yMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c2).min, dataSubFCS.columnMinMax(c2).max)
      // unormalized cluster centers
      val col1 = clusterMean.map(x => x._1.raw(c1)).toArray.
        map(x => x *
          pow(fcsParsed.meanSquareColTakenMap(c1) - pow(fcsParsed.meanColTakenMap(c1), 2), .5) +
          fcsParsed.meanColTakenMap(c1))
      val col2 = clusterMean.map(x => x._1.raw(c2)).toArray.
        map(x => x *
          pow(fcsParsed.meanSquareColTakenMap(c2) - pow(fcsParsed.meanColTakenMap(c2), 2), .5) +
          fcsParsed.meanColTakenMap(c2))
      val totalSize = clusterSize.map(_._2.toDouble).sum
      xyplot(
        Mat(Vec(col1), Vec(col2),
          Vec(clusterSize.map(x => x._1.toDouble).toArray),
          Vec(clusterSize.map(x => 10 * log10(x._2.toDouble) / log10(totalSize.toDouble)).toArray)) ->
          point(
            labelText = false,
            color = DiscreteColors(kMeanR.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = clusterSize.toArray.map(
          x =>
            (x._1 + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(kMeanR.means.length - 1)(x._1.toDouble))),
        xlab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c1) + "N")
        },
        ylab = try fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "S") catch {
          case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + fcsParsed.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))

  }

  def kMeanFCSPlotSeqEuclid(kmeanEuclid: ParArray[(List[Double], KMeansResult)])
  = {
    val min4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).min * .95
    val max4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).max * 1.05
    val mat4Plot = Mat((kmeanEuclid.map(_._1.toArray).toList :::
      List((0 until kmeanEuclid.map(_._1.toArray).toArray.head.length).toArray.map(_.toDouble))).toArray)
    xyplot(mat4Plot -> (0 until mat4Plot.numCols - 1).map(x => line(yCol = x, xCol = mat4Plot.numCols - 1,
      color = DiscreteColors(mat4Plot.numCols - 2)(x.toDouble))).toList)(
      ylim = Option(min4Plot, max4Plot), xlim = Option(0.0, (mat4Plot.numRows - 1).toDouble))
  }

  def plotKSeqToPng(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
                    fileName: String, widthPng: Int = 1000) = {
    val filePng = new File(fileName)
    pngToFile(filePng, plotSeq.build, widthPng)
  }

  def writeClusterSizeCsv(kMeanCluster: org.saddle.Vec[Int], fileName: String) = {
    val clusterSize = kMeanCluster.toArray.groupBy(identity).map(x => (x._1, x._2.size))
    val clusterFrame = Frame("Cluster" -> Vec(clusterSize.map(_._1 + 1).toArray), "Size" -> Vec(clusterSize.map(_._2).toArray))
    clusterFrame.writeCsvFile(fileName)
  }
}