
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
import ClusterEllipse._
import org.saddle.io.CsvImplicits._


/** class for transforming fcs paramters (data)
  *
  * @param index index of transformed paramter, start at 1
  * @param appFunction appFunction == null -> no tranformation,
  * @param offset appFuction(x-min(x)+offset), if offset == null appFunction(x)
  */
case class TransformParam(index : Int, appFunction : (Double => Double),offset : Double ) {}

/** for inputs for parsing fcs file
  *
  * @param file
  * @param takeParameter
  * @param takeNbEvent
  */
case class FCSInputFull(file: String, takeParameter: List[TransformParam], takeNbEvent: Int) {}

/**
  * old structure for FCSParserFull, takeParameters are indices (start at 1), log ? , min for Log, deprecated */
case class FCSInputFull_old(file: String, takeParameter: List[(Int, Boolean, Double)], takeNbEvent: Int) {}

// case class KMeanEuclid() is missing, should do it once

/** structure for kmean inputs of FCS
  *
  *
  * @param clusterNb
  * @param takeRows array of indices (start at 0)
  * @param iterations
  * @param seedK seed of random for initial condition
  */
case class KMeanFCSInput(clusterNb: Int = 5, takeRows: Array[Int] = (0 until 100).toArray, iterations: Int = 100, seedK: Int = 0) {}
object KMeanFCSInput {
  def apply(clusterNb: Int, nbRows: Int, iterations: Int, seedK: Int) : KMeanFCSInput =
    new KMeanFCSInput(clusterNb, (0 until nbRows).toArray, iterations: Int, seedK: Int)
}

/** Multiple KMean results with data
  * clusters are based on normalized data.
  * @param textSegmentMap
  * @param takenParam
  * @param meanCol
  * @param sdCol
  * @param dataMat
  * @param euclidKResult
  */
case class FCSDataParKMean(textSegmentMap: Map[String, String],
                           takenParam: scala.collection.immutable.IndexedSeq[Int],
                           meanCol: Array[Double], sdCol: Array[Double], dataMat: Mat[Double],
                           euclidKResult: ParArray[(List[Double], KMeansResult)] ) {
}

/** Final results of KMean with data, used for plotting
  * clusters are based on normalized data.
  * @param textSegmentMap
  * @param takenParam
  * @param meanCol
  * @param sdCol
  * @param dataMat
  * @param bestKMean
  */
case class FCSDataFinalKMean(textSegmentMap : Map[String, String],
                             takenParam: scala.collection.immutable.IndexedSeq[Int],
                             meanCol: Array[Double], sdCol: Array[Double], dataMat: Mat[Double],
                             bestKMean : KMeansResult) {
  /** overloading constructor
    *
    * @param fcsDataParKMean
    * @return
    */
  def this(fcsDataParKMean : FCSDataParKMean) = this(fcsDataParKMean.textSegmentMap,
    fcsDataParKMean.takenParam,
    fcsDataParKMean.meanCol,
    fcsDataParKMean.sdCol,
    fcsDataParKMean.dataMat,
    fcsDataParKMean.euclidKResult.toArray.
      filter(y => (y._1.last == (fcsDataParKMean.euclidKResult.toArray.map(x => x._1.last).min))).head._2)

  def subClustering(subClusterIndex : Int, subCluster : KMeansResult) : FCSDataFinalKMean = {
    if (subCluster.means.length < 2) sys.error("Sub cluster has les than two elements")
    val subClusterDataIndices = bestKMean.clusters.toSeq.zipWithIndex.filter(x => (x._1 == subClusterIndex)).map(_._2)
    if (subClusterDataIndices.length != subCluster.clusters.length) sys.error("Sub clustering has the wrong size")
    val subClusterIdList : List[Int] = List(subClusterIndex) :::
      (bestKMean.means.length until (subCluster.means.length + bestKMean.means.length-1)).toList
    FCSDataFinalKMean(textSegmentMap,takenParam,meanCol,sdCol,dataMat,KMeansResult(,))
  }
}

/** companion object for overloading case class constructor
  *
  */
object FCSDataFinalKMean {
  /** overloading case class constructor
    *
    * @param fcsDataParKMean
    * @return
    */
  def apply(fcsDataParKMean : FCSDataParKMean) = {new FCSDataFinalKMean(fcsDataParKMean)}
}

/** class for reading header of fcs
  *
  * @param fcsNameInput
  */
class FCSHeader(fcsNameInput: String) {
  val fcsFile = new String(fcsNameInput)
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

  /**
    * reading text segment
    */
  private val fcsTextSegment =
    (for (i <- binaryFileIndex to lastTextSegment) yield fcsFileBuffer.read).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1
  val fcsTextSegmentMap: Map[String, String] = FCSParserFull.textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE") + (if (fcsTextSegmentMap("$MODE") == "L") " OK" else " can't get data"))
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

  // method for constructing FCSInputFull from prompt
 /* def getOnlineFCSInput_old: FCSInputFull_old = {
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
    FCSInputFull_old(fcsFile, tParam.toList, tNbEvent)
  }*/

  /**
    * method for constructing FCSInputFull from prompt
    * @return
    */
  def getOnlineFCSInput: FCSInputFull = {
    val tParam: List[TransformParam] = (1 to nbPar).map(param => {
      val paramName = fcsTextSegmentMap("$P" + param + "N") +
        (if (fcsTextSegmentMap.contains("$P" + param + "S")) " -> " + fcsTextSegmentMap("$P" + param + "S") else "")
      println()
      print(paramName)
      if (!(scala.io.StdIn.readLine(", Take [y]/n? ") == "n")) {
        (scala.io.StdIn.readLine("Apply function: [Nothing], (l)og or (h)yperbolic sine? ")) match {
          case "l" => {
            val oVal = scala.io.StdIn.readLine ("Offset value (default 1.0): ") match {
              case "" => 1.0
              case resp: String => try {
                resp.toDouble
              } catch {
                case _: Throwable => {
                  println("Take 1.0"); 1.0
                }
              }
            }
            TransformParam(param, (x => log10(x)) : (Double => Double), if (oVal <= 0.0 ) {println("Take 1.0");1.0
            } else oVal)
          }
          case "h" => {TransformParam(param, (x => asinh(x) ):(Double => Double), NaN)}
          case _: String => TransformParam(param, null, NaN)
        }
      } else null
    }).filter(_ != null).toList

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
    FCSInputFull(fcsFile, tParam, tNbEvent)
  }
}

/** Companion object of FCSParserFull
  *
  */
object FCSParserFull {
  val offsetByteText: (Int, Int, Int) = (10, 17, 25)
  val offsetByteAnalysis: (Int, Int, Int) = (42, 49, 57)
  val defaultMinLog = 0.0

  /** method for reading parameter map
    *
    * @param inList
    * @return
    */
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

/** Parsed fcs file
  *
  * @param fcsInput
  */
class FCSParserFull(fcsInput: FCSInputFull) {
  private val fcsFile = new String(fcsInput.file)
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
  private val nbPar: Int = fcsTextSegmentMap("$PAR").toInt
  //val nbEvent: Int = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  val nbEvent: Int = fcsInput.takeNbEvent
  private val bitToFloat: List[Int] = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList

  val takenParam: scala.collection.immutable.IndexedSeq[Int] = fcsInput.takeParameter.map(_.index).toIndexedSeq //carefull, start at 1

  for (i <- binaryFileIndex until firstDataSegment) yield fcsFileBuffer.read
  binaryFileIndex = firstDataSegment
  println("Size of data: " + fcsInput.takeNbEvent * nbPar)

  private val dataByteArrayFCS = new Array[Byte](bitToFloat.sum / 8 * fcsInput.takeNbEvent)
  for (indexByte <- (0 until bitToFloat.sum / 8 * fcsInput.takeNbEvent)) {
    dataByteArrayFCS(indexByte) = fcsFileBuffer.read.toByte
    binaryFileIndex += 1
  }

  private val spillArray = fcsTextSegmentMap("SPILL").split(",")
  val nbCompParam = spillArray.head.toInt
  private val indexCompParam = (1 to nbCompParam).map(x => {
    val regName = "P(.*)N".r
    regName.findFirstMatchIn(fcsTextSegmentMap.find(keyVal => keyVal._2 == spillArray(x)).get._1).get.group(1).toInt
  }).map(_ - 1) // index should start a 0

  val compMatrix: Array[Double] = inv(new DenseMatrix(nbCompParam, nbCompParam, spillArray.drop(nbCompParam + 1).map(_.toDouble)).t).toArray

  private val byteTakeList = bitToFloat.
    foldLeft(List[Int](0))((x, y) => (x.head + y / 8) :: x).tail.reverse.zip(bitToFloat).map(x => (x._1 until (x._1 + x._2 / 8))).
    toArray

  //private val dataTakenArrayFCSNotC =
  //  new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  private val dataTakenArrayFCS =
    new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  private val dataNormalizedTakenArrayFCS =
    new Array[Double](fcsInput.takeNbEvent * takenParam.length)
  println("Get data and compensate")
  (0 until fcsInput.takeNbEvent).toParArray.foreach(event => {
    val nbParam = takenParam.length
    val eventNbByte = bitToFloat.sum / 8
    val lineArray: Array[Double] = (0 until nbPar).map(par => {
      if (byteTakeList(par).length == 4) {
        var tempByteBuffer = ByteBuffer.wrap(byteTakeList(par).map(x => dataByteArrayFCS(event * eventNbByte + x)).toArray)
        if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") tempByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
        tempByteBuffer.getFloat.toDouble
      }
      else {
        var tempByteBuffer = ByteBuffer.wrap(byteTakeList(par).map(x => dataByteArrayFCS(event * eventNbByte + x)).toArray)
        if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") tempByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
        tempByteBuffer.getDouble
      }
    }).toArray
    takenParam.zipWithIndex.foreach(param => { // careful, takenParam start at 1
      //dataTakenArrayFCSNotC(event * nbParam + (param._2)) = lineArray(param._1 - 1)
      if (indexCompParam.contains(param._1 - 1)) {
        val indexCompMatrixRow = indexCompParam.indexOf(param._1 - 1)
        dataTakenArrayFCS(event * nbParam + (param._2)) =
          indexCompParam.zipWithIndex.map(compParam => {
            compMatrix(indexCompMatrixRow * nbCompParam + compParam._2) * lineArray(compParam._1)
          }).sum
      }
      else dataTakenArrayFCS(event * nbParam + (param._2)) = lineArray(param._1 - 1)
    })
  })
  println("Apply function and normalize")

  // parallelize
  private val mean_sdCol: Array[(Int, Double, Double)] = fcsInput.takeParameter.zipWithIndex.toParArray.map(takeParamZipindex => {
    val nbParam = takenParam.length
    var tmpSum = 0d
    var tmpSumSq = 0d
    if (takeParamZipindex._1.appFunction != null) {
      val tmpMin = if (!takeParamZipindex._1.offset.isNaN)
        (0 until fcsInput.takeNbEvent).map(event => dataTakenArrayFCS(event * nbParam + takeParamZipindex._2)).min - takeParamZipindex._1.offset
      else 0
      (0 until fcsInput.takeNbEvent).foreach(event => {
        val functionValue = takeParamZipindex._1.appFunction(dataTakenArrayFCS(event * nbParam + takeParamZipindex._2) - tmpMin)
        tmpSum += functionValue
        tmpSumSq += functionValue * functionValue
        dataTakenArrayFCS(event * nbParam + takeParamZipindex._2) = functionValue
      })
    } else {
      (0 until fcsInput.takeNbEvent).foreach(event => {
        val noFunctionValue = dataTakenArrayFCS(event * nbParam + takeParamZipindex._2)
        tmpSum += noFunctionValue
        tmpSumSq += noFunctionValue * noFunctionValue
      })
    }
    val eventMean: Double = tmpSum / fcsInput.takeNbEvent
    val eventSD: Double = pow((tmpSumSq / fcsInput.takeNbEvent - eventMean * eventMean), .5) //bias variance
    for (event <- (0 until fcsInput.takeNbEvent)) {
      dataNormalizedTakenArrayFCS(event * nbParam + takeParamZipindex._2) =
        (dataTakenArrayFCS(event * nbParam + takeParamZipindex._2) - eventMean) / eventSD
    }
    (takeParamZipindex._2, eventMean, eventSD)
  }).toArray.sortBy(x => x._1) // sort needed when parallelized


  val meanColTakenMap: Array[Double] = mean_sdCol.map(x => x._2)
  val sdColTakenMap: Array[Double] = mean_sdCol.map(x => x._3)

  val dataTakenMatFCS: Mat[Double] = Mat(fcsInput.takeNbEvent, takenParam.length, dataTakenArrayFCS)
  //val dataTakenMatFCSNotC: Mat[Double] = Mat(fcsInput.takeNbEvent, takenParam.length, dataTakenArrayFCSNotC)
  val dataNormalizedTakenMatFCS: Mat[Double] = Mat(fcsInput.takeNbEvent, takenParam.length, dataNormalizedTakenArrayFCS)

  def getTakenMatrixFCS: DenseMatrix[Double] = {
    val FCSMatrix = new DenseMatrix[Double](fcsInput.takeNbEvent, takenParam.length)
    for (rowFCS <- (0 until fcsInput.takeNbEvent); colFCS <- takenParam.indices) {
      FCSMatrix(rowFCS, colFCS) = dataTakenArrayFCS(colFCS + rowFCS * takenParam.length)
    }
    FCSMatrix
  }

  /** kmean clustering
    *
    * @param kMeanFCSInput
    * @return
    */
  private def kmeanFCS(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    val dataSubFCS = dataNormalizedTakenMatFCS.row(kMeanFCSInput.takeRows)
    val rand4K = new Random(kMeanFCSInput.seedK)
    val dataInitK = dataSubFCS.row((1 to kMeanFCSInput.clusterNb).
      map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
    kmeans.apply(dataSubFCS, dataInitK, kMeanFCSInput.iterations)
  }

  /** kmean++ clustering
    *
    * @param kMeanFCSInput
    * @return
    */
  def kmeanPPFCS(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
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
    val initDataIndex = rand4K.nextInt(kMeanFCSInput.takeRows.length)
    val clusterIndices = initClust(List(initDataIndex),
      kMeanFCSInput.takeRows.filter(x => x != initDataIndex),
      kMeanFCSInput.clusterNb - 1, rand4K).toArray
    kmeans.apply(dataNormalizedTakenMatFCS.row(kMeanFCSInput.takeRows),
      dataNormalizedTakenMatFCS.row(clusterIndices), kMeanFCSInput.iterations)
  }

  /** kmean clustering, several steps and several attempts paralleled), with euclid norm quality
    *
    * @param kMeanFCSInput
    * @param stepK
    * @param seedArrayK
    * @return
    */
  def kmeanFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  FCSDataParKMean = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) Nil else {
        print("Step " + step + "\r")
        val stepKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row(takeRows),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataNormalizedTakenMatFCS.row(takeRows))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / takeRows.length / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, takeRows, iterations, step - 1)
      }
    }

    FCSDataParKMean(fcsTextSegmentMap, takenParam, meanColTakenMap, sdColTakenMap,
      dataTakenMatFCS.row(kMeanFCSInput.takeRows),
      seedArrayK.map(seedKFromArray => {
        val rand4K = new Random(seedKFromArray)
        val dataInitK = dataNormalizedTakenMatFCS.row(kMeanFCSInput.takeRows).
          row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
        val listEuclidRand = listEuclid(dataInitK.rows, kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
        print("Finish seed " + seedKFromArray + "\r")
        (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
      }))
  }

  /** kmeanFCSEuclid to be continued from result of kmeanFCSEuclid
    *
    * @param kMeanFCSInput
    * @param stepK
    * @param previousEuclid
    * @return
    */
  def kmeanFCSEuclidConvContinue(kMeanFCSInput: KMeanFCSInput, stepK: Int, previousEuclid: ParArray[(List[Double], KMeansResult)]):
  FCSDataParKMean = {
    //same inner function as above (not very clean...)
    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) Nil else {
        print("Step " + step + "\r")
        val stepKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row(takeRows),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataNormalizedTakenMatFCS.row(takeRows))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / takeRows.length / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, takeRows, iterations, step - 1)
      }
    }

    new FCSDataParKMean(fcsTextSegmentMap, takenParam, meanColTakenMap, sdColTakenMap,
      dataTakenMatFCS.row(kMeanFCSInput.takeRows),
      previousEuclid.map(euclid => {
        val listEuclidNew = listEuclid(euclid._2.means, kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
        (euclid._1 ::: listEuclidNew.map(_._1), listEuclidNew.last._2)
      }))
  }

  /** kmean++ clustering, several steps and several attempts (paralleled), with euclid norm quality
    *
    * @param kMeanFCSInput
    * @param stepK
    * @param seedArrayK
    * @return
    */
  def kmeanPPFCSEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  FCSDataParKMean = { // careful: it correspond to iterations*stepK + (stepk -1) or something like that
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

    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) Nil else {
        println("Step " + step)
        val stepKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row(takeRows),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataNormalizedTakenMatFCS.row(takeRows))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / takeRows.length / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, takeRows, iterations, step - 1)
      }
    }
    FCSDataParKMean(fcsTextSegmentMap, takenParam, meanColTakenMap, sdColTakenMap,
      dataTakenMatFCS.row(kMeanFCSInput.takeRows),
    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val initDataIndex = rand4K.nextInt(kMeanFCSInput.takeRows.length)
      val clusterIndices = initClust(List(initDataIndex),
        kMeanFCSInput.takeRows.filter(x => x != initDataIndex).toArray,
        kMeanFCSInput.clusterNb - 1, rand4K).toArray
      val listEuclidRand = listEuclid(dataNormalizedTakenMatFCS.row(clusterIndices).rows,
        kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
      (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
    }))
  }

  /** kmean clustering, several steps and several attemps
    *
    * @param kMeanFCSInput
    * @param stepK
    * @param seedArrayK
    * @return
    */
  def kmeanFCSTestConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: Array[Int]):
  Array[List[IndexedSeq[Vec[Double]]]] = {
    def listMeanKMeans(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int)
    : List[IndexedSeq[Vec[Double]]] = {
      if (step == 0) Nil else {
        val meanKMeans = kmeans.apply(dataNormalizedTakenMatFCS.row(takeRows),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations).means
        meanKMeans :: listMeanKMeans(meanKMeans, takeRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val dataInitK = dataNormalizedTakenMatFCS.row(kMeanFCSInput.takeRows).
        row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
      listMeanKMeans(dataInitK.rows, kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
    })
  }

  /** clustering from given ellipses
    * use only paramters with identical names between data and ellipses
    * @param clusterListParam Param given in an Array[String]
    * @return
    */
  def fcsDataFinalClusterFromEllipse(clusterListParam: (List[EllipseClusterId], Array[String])): FCSDataFinalKMean = {
    val paramIndexClData = clusterListParam._2.zipWithIndex.map(ellParamInd =>
      takenParam.zipWithIndex.map(tParamInd => {
        if (ellParamInd._1 == fcsTextSegmentMap("$P" + tParamInd._1 + "N")) (ellParamInd._2, tParamInd._2) else (-1, -1)
      })).flatMap(x => x).filter(ind => (ind._1 > -1))
    val clIndex :Array[Int]= paramIndexClData.map(_._1)
    val dataIndex = paramIndexClData.map(_._2)
    val listSubClusterId = clusterListParam._1.map(elClusterId =>
      EllipseClusterId(EllipseCluster(elClusterId.cluster.size,
        clIndex.map(ind => elClusterId.cluster.mean(ind)),
        elClusterId.cluster.varMat(clIndex.toSeq,clIndex.toSeq).toDenseMatrix), elClusterId.clusterId))
    val cluster4KMean = (0 until nbEvent).map(event => {
      val elDistIdList = listSubClusterId.map(elClusterId => {
        (ClusterEllipse.distEllipseCluster(dataTakenMatFCS.col(dataIndex).row(event), elClusterId.cluster), elClusterId.clusterId)
      })
      val minDist = min(elDistIdList.map(_._1))
      elDistIdList.filter(x => (x._1 == minDist)).head._2
    })
    val mean4KMean = cluster4KMean.zipWithIndex.groupBy(_._1).map(x => (x._1, x._2.map(y => y._2))).
      map(clusterIdIndices => (0 until takenParam.length).map(param => dataNormalizedTakenMatFCS.col(param).toArray).
        map(col => {
          val size4Mean = clusterIdIndices._2.length
          clusterIdIndices._2.map(index => col(index)).toArray.sum / size4Mean
        })).map(x => Vec(x.toArray)).toArray
    FCSDataFinalKMean(fcsTextSegmentMap,
      takenParam,
      meanColTakenMap, sdColTakenMap, dataTakenMatFCS,
      new KMeansResult(Vec(cluster4KMean.toArray), mean4KMean))
  }
}
