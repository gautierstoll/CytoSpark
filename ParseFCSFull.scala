
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

  // method for reading parameter map
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

// structure for kmean of FCS
case class KMeanFCSInput(clusterNb: Int = 5, nbRows: Int = 100, iterations: Int = 100, seedK: Int = 0) {}

// case class KMeanEuclid() should do this once


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
  // parrelized with ParArray, eg
  // Mat(tmpArray.zipWithIndex.groupBy(x=> x._2%3).map(x=> (x._1,x._2.map(_._1))).toParArray.map(x=>(x._1,x._2.map(y=>y-x._2.min))).toArray.sortBy(_._1).map(_._2))

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
  print("Mean values\t")
  meanColTakenMap.keys.toArray.sorted.foreach(x => print(meanColTakenMap(x) + "\t"))
  println()
  print("SD\t")
  meanColTakenMap.keys.toArray.sorted.
    foreach(x => print(pow(meanSquareColTakenMap(x) - meanColTakenMap(x) * meanColTakenMap(x), .5).toString + "\t"))
  println()
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

  def getTakenMatrixFCS: DenseMatrix[Double] = {
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

  // kmean clustering, several steps and several attempts paralleled), with euclid norm quality
  def kmeansFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
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

  def kmeanFCSEuclidConvContinue(kMeanFCSInput: KMeanFCSInput, stepK: Int, previousEuclid: ParArray[(List[Double], KMeansResult)]):
  ParArray[(List[Double], KMeansResult)] = {
    //same inner function as above (not very clean...)
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

    previousEuclid.map(euclid => {
      val listEuclidNew = listEuclid(euclid._2.means, kMeanFCSInput.nbRows, kMeanFCSInput.iterations, stepK)
      (euclid._1 ::: listEuclidNew.map(_._1), listEuclidNew.last._2)
    })
  }

  // kmean++ clustering, several steps and several attempts (paralleled), with euclid norm quality
  def kmeansPPFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
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

class FCSParserFullFast(fcsInput: FCSInputFull) {
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

  private val dataByteArrayFCS = new Array[Byte](bitToFloat.sum / 8 * fcsInput.takeNbEvent)
  for (indexByte <- (0 until bitToFloat.sum / 8 * fcsInput.takeNbEvent)) {
    dataByteArrayFCS(indexByte) = fcsFileBuffer.read.toByte
    binaryFileIndex += 1
  }

  val byteTakeList = bitToFloat.
    foldLeft(List[Int](0))((x, y) => (x.head + y / 8) :: x).tail.reverse.zip(bitToFloat).map(x => (x._1 until (x._1 + x._2 / 8))).
    toArray
  val takenByteTakeList = takenParam.map(x => byteTakeList(x))
  val parralelParamByteListLogMin = takenParam. //._1: Parameter index (order the will be used after parralel action)
    zip(takenByteTakeList). // ._2 Byte that are taken in Byte Arra
    zip(fcsInput.takeParameter.map(_._2)).map(x => (x._1._1, x._1._2, x._2)). //._3 log?
    zip(fcsInput.takeParameter.map(_._3)).map(x => (x._1._1, x._1._2, x._1._3, x._2)).toParArray //._4 minimum if log is taken


  private val dataTakenArrayFCS =
    new Array[Double](fcsInput.takeNbEvent * takenParam.length)

  parralelParamByteListLogMin.foreach(paramByteListLogMin => {
    val eventNbByte = bitToFloat.sum / 8
    val nbParam = takenParam.length
    val byteLength = paramByteListLogMin._2.length
    if (paramByteListLogMin._3) {
      var tmpMin = Double.MaxValue
      for (event <- (0 until fcsInput.takeNbEvent)) {
        if (byteLength == 4) {
          var tempByteBuffer = ByteBuffer.wrap(paramByteListLogMin._2.map(x => dataByteArrayFCS(event * eventNbByte + x)).toArray)
          if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") tempByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
          val doubleFromBytes = tempByteBuffer.getFloat.toDouble
          dataTakenArrayFCS(event * nbParam + paramByteListLogMin._1) = doubleFromBytes
          tmpMin = doubleFromBytes.min(tmpMin)
        }
        else {
          var tempByteBuffer = ByteBuffer.wrap(paramByteListLogMin._2.map(x => dataByteArrayFCS(event * eventNbByte + x)).toArray)
          if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") tempByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
          val doubleFromBytes = tempByteBuffer.getDouble
          dataTakenArrayFCS(event * nbParam + paramByteListLogMin._1) = doubleFromBytes
          tmpMin = doubleFromBytes.min(tmpMin)
        }
      }

      var tmpSum = 0
      var tmpSumSq = 0
      for (event <- (0 until fcsInput.takeNbEvent)) {
        val logValue = log10(dataTakenArrayFCS(event * nbParam + paramByteListLogMin._1) - tmpMin +
          pow(10, paramByteListLogMin._4))
        tmpSum += logValue
        tmpSumSq += logValue*logValue
        dataTakenArrayFCS(event * nbParam + paramByteListLogMin._1) = logValue
      }
    }
    else {

    }
  })

  //  for (index <- bitToFloat.sum / 8 * fcsInput.takeNbEvent) {}
  //
  //  private val dataTakenArrayFCS =
  //    new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  //  for (indexFCS <- 0 until fcsInput.takeNbEvent * nbPar) {
  //    print(indexFCS + "\r")
  //    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 32) {
  //      binaryFileIndex += 4
  //      var tempFileArray = ByteBuffer.wrap((1 to 4).map(x => fcsFileBuffer.read.toByte).toArray)
  //      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
  //        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
  //      }
  //      if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
  //        val takenIndex = takenParam.indices.
  //          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
  //        val readValue = tempFileArray.getFloat.toDouble
  //        val takenArrayIndex: Int = takenParam.indices.
  //          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
  //          (indexFCS / nbPar) * takenParam.length
  //        dataTakenArrayFCS(takenArrayIndex) = readValue
  //      }
  //    }
  //    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 64) {
  //      binaryFileIndex += 8
  //      val tempFileArray = ByteBuffer.wrap((1 to 8).map(x => fcsFileBuffer.read.toByte).toArray)
  //      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
  //        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
  //      }
  //      if (takenParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
  //        val takenIndex = takenParam.indices.
  //          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head
  //        val readValue = tempFileArray.getDouble
  //        val takenArrayIndex = takenParam.indices.
  //          filter(x => (takenParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
  //          (indexFCS / nbPar) * takenParam.length
  //        dataTakenArrayFCS(takenArrayIndex) = readValue
  //      }
  //    }
  //  }
  // parrelized with ParArray, eg
  // Mat(tmpArray.zipWithIndex.groupBy(x=> x._2%3).map(x=> (x._1,x._2.map(_._1))).toParArray.map(x=>(x._1,x._2.map(y=>y-x._2.min))).toArray.sortBy(_._1).map(_._2))

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
  print("Mean values\t")
  meanColTakenMap.keys.toArray.sorted.foreach(x => print(meanColTakenMap(x) + "\t"))
  println()
  print("SD\t")
  meanColTakenMap.keys.toArray.sorted.
    foreach(x => print(pow(meanSquareColTakenMap(x) - meanColTakenMap(x) * meanColTakenMap(x), .5).toString + "\t"))
  println()
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

  def getTakenMatrixFCS: DenseMatrix[Double] = {
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

  // kmean clustering, several steps and several attempts paralleled), with euclid norm quality
  def kmeansFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
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

  def kmeanFCSEuclidConvContinue(kMeanFCSInput: KMeanFCSInput, stepK: Int, previousEuclid: ParArray[(List[Double], KMeansResult)]):
  ParArray[(List[Double], KMeansResult)] = {
    //same inner function as above (not very clean...)
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

    previousEuclid.map(euclid => {
      val listEuclidNew = listEuclid(euclid._2.means, kMeanFCSInput.nbRows, kMeanFCSInput.iterations, stepK)
      (euclid._1 ::: listEuclidNew.map(_._1), listEuclidNew.last._2)
    })
  }

  // kmean++ clustering, several steps and several attempts (paralleled), with euclid norm quality
  def kmeansPPFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
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
