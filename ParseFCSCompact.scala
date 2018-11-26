
import java.io._
import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.{Paths, Files}
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
import stat.sparse.SVec
import scala.collection.parallel.mutable._
import org.saddle.io.CsvImplicits._


class FCSParserCompact(fcsNameInput: String, minValCytInput: Double) {

  private val offsetByteText: (Int, Int, Int) = (10, 17, 25)
  private val offsetByteAnalysis: (Int, Int, Int) = (42, 49, 57)

  private def textSegmentMap(inList: List[Byte]): Map[String, String] = {
    def lengthSecondCharSep(inList: List[Byte]): Int = {
      def dropUntilSinglSep(ByteSeparator: Byte, offsetcharList: Int, charList: List[Byte]): Int = {
        val newOffsetcharList = offsetcharList + charList.drop(offsetcharList).takeWhile(_ != ByteSeparator).length
        charList.drop(newOffsetcharList) match {
          case ByteSeparator :: Nil => newOffsetcharList // two separators is not a separator
          case ByteSeparator :: ByteSeparator :: yy => dropUntilSinglSep(ByteSeparator, newOffsetcharList + 2, charList)
          case ByteSeparator :: yy => newOffsetcharList
          case yy => {
            sys.error("Error in Parsing text segment")
            0
          }
        }
      }

      dropUntilSinglSep(inList.head, 1, inList)
    }

    val keyLength = lengthSecondCharSep(inList) - 1
    val valLength = lengthSecondCharSep(inList.drop(keyLength + 1)) - 1
    if (inList.length <= (keyLength + valLength + 3)) {
      Map(inList.slice(1, 1 + keyLength).map(_.toChar).mkString("") ->
        inList.slice(1 + keyLength + 1, 1 + keyLength + 1 + valLength).map(_.toChar).mkString(""))
    }
    else {
      Map(inList.slice(1, 1 + keyLength).map(_.toChar).mkString("") ->
        inList.slice(1 + keyLength + 1, 1 + keyLength + 1 + valLength).map(_.toChar).mkString("")) ++
        textSegmentMap(inList.drop(1 + keyLength + 1 + valLength))
    }
  }


  val fcsFile = new String(fcsNameInput)
  val minValCyt: Double = minValCytInput
  if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " not found")

  private val fcsFileBuffer =
    new BufferedInputStream(new FileInputStream(fcsFile))

  private var binaryFileIndex: Int =
    0
  for (i <- 0 until offsetByteText._1) {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstTextSegment =
    (for (i <- offsetByteText._1 to offsetByteText._2) yield {
      fcsFileBuffer.read
    }).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteText._2 + 1

  private val lastTextSegment =
    (for (i <- binaryFileIndex to offsetByteText._3) yield {
      fcsFileBuffer.read
    }).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteText._3 + 1
  for (i <- binaryFileIndex until offsetByteAnalysis._1) yield {
    fcsFileBuffer.read
    binaryFileIndex += 1
  }
  private val firstAnalysisSegment =
    (for (i <- binaryFileIndex to offsetByteAnalysis._2) yield {
      fcsFileBuffer.read
    }).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteAnalysis._2 + 1

  private val lastAnalysisSegment =
    (for (i <- binaryFileIndex to offsetByteAnalysis._3) yield {
      fcsFileBuffer.read
    }).
      map(_.toChar).filter(_ != ' ').mkString("").toInt
  binaryFileIndex = offsetByteAnalysis._3 + 1

  for (i <- binaryFileIndex until firstTextSegment) yield {
    fcsFileBuffer.read
  }
  binaryFileIndex = firstTextSegment

  private val fcsTextSegment =
    (for (i <- binaryFileIndex to lastTextSegment) yield {
      fcsFileBuffer.read
    }).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1
  val fcsTextSegmentMap: Map[String, String] = textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Byte order: " + fcsTextSegmentMap("$BYTEORD"))
  println("Number of events: " + fcsTextSegmentMap("$TOT"))

  private val firstDataSegment =
    fcsTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  private val lastDataSegment =
    fcsTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  val nbPar: Int = fcsTextSegmentMap("$PAR").toInt
  val nbEvent: Int = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  val bitToFloat: List[Int] = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList
  val compensatedParam: scala.collection.immutable.IndexedSeq[Int] =
    (1 to bitToFloat.length).filter(x => fcsTextSegmentMap.contains("$P" + x + "S"))
  compensatedParam.foreach(x => println("$P" + x + "S -> " + fcsTextSegmentMap("$P" + x + "S")))

  for (i <- binaryFileIndex until firstDataSegment) yield {
    fcsFileBuffer.read
  }
  binaryFileIndex = firstDataSegment
  println("Size of data: " + nbEvent * nbPar)
  private var dataCompensatedArrayFCS =
    new Array[Double](nbEvent * compensatedParam.length)
  for (indexFCS <- 0 until nbEvent * nbPar) {
    print(indexFCS + "\r")
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 32) {
      binaryFileIndex += 4
      var tempFileArray = ByteBuffer.wrap((1 to 4).map(x => fcsFileBuffer.read.toByte).toArray)
      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
      }
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex: Int = compensatedParam.indices.
          filter(x => (compensatedParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getFloat.toDouble - minValCyt)
      }
    }
    if (bitToFloat(indexFCS - (indexFCS / bitToFloat.length) * bitToFloat.length) == 64) {
      binaryFileIndex += 8
      var tempFileArray = ByteBuffer.wrap((1 to 8).map(x => fcsFileBuffer.read.toByte).toArray)
      if (fcsTextSegmentMap("$BYTEORD") == "1,2,3,4") {
        tempFileArray.order(ByteOrder.LITTLE_ENDIAN)
      }
      if (compensatedParam.contains(1 + indexFCS - (indexFCS / nbPar) * nbPar)) {
        val compArrayIndex = compensatedParam.indices.
          filter(x => (compensatedParam(x) == (1 + indexFCS - indexFCS / nbPar * nbPar))).head +
          (indexFCS / nbPar) * compensatedParam.length
        dataCompensatedArrayFCS(compArrayIndex) = log10(tempFileArray.getDouble - minValCyt)
      }
    }
  }
  val dataCompensatedMatFCS: Mat[Double] = Mat(nbEvent, compensatedParam.length, dataCompensatedArrayFCS)

  def getCompensatedMatrixFCS: DenseMatrix[Double] = {
    var FCSMatrix = new DenseMatrix[Double](nbEvent, compensatedParam.length)
    for (rowFCS <- (0 until nbEvent); colFCS <- compensatedParam.indices) {
      FCSMatrix(rowFCS, colFCS) = dataCompensatedArrayFCS(colFCS + rowFCS * compensatedParam.length)
    }
    FCSMatrix
  }

  def kmeansCompensated(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    val dataSubFCS = dataCompensatedMatFCS.row((kMeanFCSInput.takeRows).toArray)
    val rand4K = new Random(kMeanFCSInput.seedK)
    val dataInitK = dataSubFCS.row((1 to kMeanFCSInput.clusterNb).
      map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
    kmeans.apply(dataSubFCS, dataInitK, kMeanFCSInput.iterations)
  }

  def kmeansPPCompensated(kMeanFCSInput: KMeanFCSInput): KMeansResult = {
    def initClust(initClustListIndex: List[Int], dataArrayIndex: Array[Int], clusterNb: Int, rand4Init: Random):
    List[Int] = {
      if (clusterNb == 0) {
        initClustListIndex
      }
      else {
        val dataIndexWithMinEuclid = dataArrayIndex.zip(
          dataArrayIndex.map(dataIndex =>
            initClustListIndex.map(initClustIndex =>
              kmeans.euclid(SVec(Series(dataCompensatedMatFCS.row(initClustIndex)), dataCompensatedMatFCS.numCols),
                dataCompensatedMatFCS.row(dataIndex))).min))
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
      (kMeanFCSInput.takeRows).filter(x => x != initDataIndex).toArray,
      kMeanFCSInput.clusterNb - 1, rand4K).toArray
    //val dataIndices = (0 until kMeanFCSInput.nbRows).filter(x => !clusterIndices.contains(x)).toArray
    kmeans.apply(dataCompensatedMatFCS.row((kMeanFCSInput.takeRows).toArray),
      dataCompensatedMatFCS.row(clusterIndices), kMeanFCSInput.iterations)
  }

  def kmeansCompensatedEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // carefull: it correspond to iterations*stepK + (stepk -1) or something like that
    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) {
        Nil
      }
      else {
        print("Step " + step + "\r")
        val stepKMeans = kmeans.apply(dataCompensatedMatFCS.row((takeRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataCompensatedMatFCS.row((takeRows).toArray))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / takeRows.length / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, takeRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val dataInitK = dataCompensatedMatFCS.row((kMeanFCSInput.takeRows).toArray).
        row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
      val listEuclidRand = listEuclid(dataInitK.rows, kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
      (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
    })
  }

  def kmeansPPCompensatedEuclidConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: ParArray[Int]):
  ParArray[(List[Double], KMeansResult)] = { // carefull: it correspond to iterations*stepK + (stepk -1) or something like that
    def initClust(initClustListIndex: List[Int], dataArrayIndex: Array[Int], clusterNb: Int, rand4Init: Random):
    List[Int] = {
      if (clusterNb == 0) {
        initClustListIndex
      }
      else {
        val dataIndexWithMinEuclid = dataArrayIndex.zip(
          dataArrayIndex.map(dataIndex =>
            initClustListIndex.map(initClustIndex =>
              kmeans.euclid(SVec(Series(dataCompensatedMatFCS.row(initClustIndex)), dataCompensatedMatFCS.numCols),
                dataCompensatedMatFCS.row(dataIndex))).min))
        val random4Index = rand4Init.nextDouble() * dataIndexWithMinEuclid.map(_._2).sum
        val indexData4Clust = dataIndexWithMinEuclid.scanLeft(0, 0d)((x, y) => (y._1, x._2 + y._2)).
          filter(x => x._2 > random4Index).head._1
        initClust(indexData4Clust :: initClustListIndex,
          dataArrayIndex.filter(x => x != indexData4Clust), clusterNb - 1, rand4Init)
      }
    }

    def listEuclid(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int):
    List[(Double, KMeansResult)] = {
      if (step == 0) {
        Nil
      }
      else {
        println("Step " + step)
        val stepKMeans = kmeans.apply(dataCompensatedMatFCS.row((takeRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations)
        (stepKMeans.clusters.toArray.zip(kmeans.matToSparse(dataCompensatedMatFCS.row((takeRows).toArray))).
          map(x => kmeans.euclid(x._2, stepKMeans.means(x._1))).sum / takeRows.length / nbPar, stepKMeans) ::
          listEuclid(stepKMeans.means, takeRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      //val dataInitK = dataCompensatedMatFCS.row((0 until kMeanFCSInput.nbRows).toArray).
      //row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.nbRows)).toArray)
      val initDataIndex = rand4K.nextInt(kMeanFCSInput.takeRows.length)
      val clusterIndices = initClust(List(initDataIndex),
        (kMeanFCSInput.takeRows).filter(x => x != initDataIndex).toArray,
        kMeanFCSInput.clusterNb - 1, rand4K).toArray
      val listEuclidRand = listEuclid(dataCompensatedMatFCS.row(clusterIndices).rows,
        kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
      (listEuclidRand.map(x => x._1), listEuclidRand.last._2)
    })
  }


  def kmeansCompensatedTestConv(kMeanFCSInput: KMeanFCSInput, stepK: Int, seedArrayK: Array[Int]):
  Array[List[IndexedSeq[Vec[Double]]]] = {
    def listMeanKMeans(initKMeans: IndexedSeq[Vec[Double]], takeRows: Array[Int], iterations: Int, step: Int)
    : List[IndexedSeq[Vec[Double]]] = {
      if (step == 0) {
        Nil
      }
      else {
        val meanKMeans = kmeans.apply(dataCompensatedMatFCS.row((takeRows).toArray),
          Mat(initKMeans.length, initKMeans.head.length,
            initKMeans.flatMap(_.toArray).toArray), iterations).means
        meanKMeans :: listMeanKMeans(meanKMeans, takeRows, iterations, step - 1)
      }
    }

    seedArrayK.map(seedKFromArray => {
      val rand4K = new Random(seedKFromArray)
      val dataInitK = dataCompensatedMatFCS.row((kMeanFCSInput.takeRows).toArray).
        row((1 to kMeanFCSInput.clusterNb).map(x => rand4K.nextInt(kMeanFCSInput.takeRows.length)).toArray)
      listMeanKMeans(dataInitK.rows, kMeanFCSInput.takeRows, kMeanFCSInput.iterations, stepK)
    })
  }
}

// already defined in ParseFCSFull
//case class KMeanFCSInput(clusterNb: Int = 5, nbRows: Int = 100, iterations: Int = 100, seedK: Int = 0) {}

//already defined in ParseFCSFull
//object FCSOutput {
//  def kMeanFCSPlot2D(fcsParsed: FCSParserCompact, kMeanR: KMeansResult, exludeCluster: Array[Int] = Array())
//  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
//    val keepIndex = (0 until kMeanR.clusters.length).
//      filter(x => (!exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
//    val dataSubFCS = fcsParsed.dataCompensatedMatFCS.row(keepIndex)
//    val subKMeanR = KMeansResult(
//      clusters = kMeanR.clusters.filter(x => !(exludeCluster.contains(x))),
//      means = kMeanR.means
//    )
//    val projections = fcsParsed.compensatedParam.indices.combinations(2).map { g =>
//      val c1 = g(0)
//      val c2 = g(1)
//      val xMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c1).min, dataSubFCS.columnMinMax(c1).max)
//      val yMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c2).min, dataSubFCS.columnMinMax(c2).max)
//      val col1 = dataSubFCS.col(c1)
//      val col2 = dataSubFCS.col(c2)
//      xyplot(
//        Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)) -> point(
//          labelText = false, size = 4.0 / log10(kMeanR.clusters.length),
//          color = DiscreteColors(kMeanR.means.length - 1)))(
//        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
//        extraLegend = subKMeanR.clusters.toArray.distinct.map(
//          x =>
//            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
//              color = DiscreteColors(kMeanR.means.length - 1)(x.toDouble))),
//
//        xlab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c1),
//        ylab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c2)
//      )
//    }
//    sequence(projections.toList, TableLayout(4))
//  }
//
//  def kMeanFCSPlotClusters2D(fcsParsed: FCSParserCompact, kMeanR: KMeansResult, exludeCluster: Array[Int] = Array())
//  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
//    val keepIndex = (0 until kMeanR.clusters.length).
//      filter(x => (!exludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
//    val dataSubFCS = fcsParsed.dataCompensatedMatFCS.row(keepIndex)
//    val clusterSize = kMeanR.clusters.toArray.groupBy(x => x).map(x => (x._1, x._2.length)).
//      filter(x => (!exludeCluster.contains(x._2))).toList.sortBy(_._1)
//    val clusterMean = kMeanR.means.zipWithIndex.filter(x => (!exludeCluster.contains(x._2)))
//    val projections = kMeanR.means.head.toArray.indices.combinations(2).map { g =>
//      val c1 = g(0)
//      val c2 = g(1)
//      val xMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c1).min, dataSubFCS.columnMinMax(c1).max)
//      val yMinMaxFCSComp = Option(dataSubFCS.columnMinMax(c2).min, dataSubFCS.columnMinMax(c2).max)
//      val col1 = clusterMean.map(x => x._1.at(c1).toDouble).toArray
//      val col2 = clusterMean.map(x => x._1.at(c2).toDouble).toArray
//      val totalSize = clusterSize.map(_._2.toDouble).sum
//      xyplot(
//        Mat(Vec(col1), Vec(col2),
//          Vec(clusterSize.map(x => x._1.toDouble).toArray),
//          Vec(clusterSize.map(x => 10 * log10(x._2.toDouble) / log10(totalSize.toDouble)).toArray)) ->
//          point(
//            labelText = false,
//            color = DiscreteColors(kMeanR.means.length - 1)))(
//        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
//        extraLegend = clusterSize.toArray.map(
//          x =>
//            (x._1 + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
//              color = DiscreteColors(kMeanR.means.length - 1)(x._1.toDouble))),
//        xlab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c1),
//        ylab = fcsParsed.compensatedParam.map(x => fcsParsed.fcsTextSegmentMap("$P" + x + "S")).toList(c2)
//      )
//    }
//    sequence(projections.toList, TableLayout(4))
//
//  }
//
//  def kMeanFCSPlotClustersConv(clusterConv: Array[List[IndexedSeq[Vec[Double]]]])
//  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
//
//    val paramLinesPlot = (0 until clusterConv.head.head.head.length).map(paramComp => {
//      val dataConvMat = Mat(((for (runIndex <- clusterConv.indices;
//                                   clusterIndex <- clusterConv.head.head.indices) yield {
//        (for (convIndex <- clusterConv.head.indices) yield {
//          clusterConv(runIndex).toArray.
//            zipWithIndex.filter(_._2 == convIndex).map(_._1).head.
//            zipWithIndex.filter(_._2 == clusterIndex).map(_._1).head.toArray.
//            zipWithIndex.filter(_._2 == paramComp).map(_._1).head
//        }).toArray
//      }).toList :::
//        List(clusterConv.head.indices.map(_.toDouble).toArray)).toArray)
//      val yLimDataConv = Option(dataConvMat.col((0 to dataConvMat.numCols - 2).toArray).toArray.min,
//        dataConvMat.col((0 to dataConvMat.numCols - 2).toArray).toArray.max)
//      val xLimDataConv = Option(0.0, (clusterConv.head.length - 1).toDouble)
//      xyplot(dataConvMat -> (0 to (dataConvMat.numCols - 2)).map(dataColIndex =>
//        line(xCol = (dataConvMat.numCols - 1), yCol = dataColIndex,
//          color = DiscreteColors(clusterConv.length - 1)((dataColIndex / clusterConv.head.head.length).toDouble),
//          //color = Color.black,
//          stroke = Stroke(2d))).toList)(xlim = xLimDataConv, ylim = yLimDataConv)
//    })
//    sequence(paramLinesPlot.toList, TableLayout(4))
//  }
//
//  def kMeanFCSPlotClustersConv2D(clusterConv: Array[List[IndexedSeq[Vec[Double]]]])
//  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
//    val param2DPlot = 0 until (clusterConv.head.head.head.length) combinations (2) map { comb2D =>
//      val dataConvMat = Mat((for (runIndex <- (0 to (clusterConv.length - 1));
//                                  clusterIndex <- (0 to (clusterConv.head.head.length - 1));
//                                  combIndex <- List(0, 1)) yield {
//        (for (convIndex <- (0 to (clusterConv.head.length - 1))) yield {
//          clusterConv(runIndex).toArray.
//            zipWithIndex.filter(_._2 == convIndex).map(_._1).head.
//            zipWithIndex.filter(_._2 == clusterIndex).map(_._1).head.toArray.
//            zipWithIndex.filter(_._2 == comb2D(combIndex)).map(_._1).head
//        }).toArray
//      }).toArray)
//      val xLimDataConv = Option(dataConvMat.col((0 to dataConvMat.numCols / 2 - 1).map(_ * 2).toArray).toArray.min,
//        dataConvMat.col((0 to dataConvMat.numCols / 2 - 1).map(_ * 2).toArray).toArray.max)
//      val yLimDataConv = Option(dataConvMat.col((0 to dataConvMat.numCols / 2 - 1).map(_ * 2 + 1).toArray).toArray.min,
//        dataConvMat.col((0 to dataConvMat.numCols / 2 - 1).map(_ * 2 + 1).toArray).toArray.max)
//      xyplot(dataConvMat -> (0 to (dataConvMat.numCols / 2 - 1)).map(dataColIndex =>
//        line(xCol = (dataColIndex * 2), yCol = dataColIndex * 2 + 1,
//          color = DiscreteColors(clusterConv.length - 1)((dataColIndex / clusterConv.head.head.length).toDouble),
//          //color = Color.black,
//          stroke = Stroke(2d))).toList)(xlim = xLimDataConv, ylim = yLimDataConv)
//    }
//    sequence(param2DPlot.toList, TableLayout(4))
//  }
//
//  def kMeanFCSPlotSeqEuclid(kmeanEuclid: ParArray[(List[Double], KMeansResult)])
//  = {
//    val min4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).min*.95
//    val max4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).max*1.05
//    val mat4Plot = Mat((kmeanEuclid.map(_._1.toArray).toList :::
//      List((0 until kmeanEuclid.map(_._1.toArray).toArray.head.length).toArray.map(_.toDouble))).toArray)
//    //println(mat4Plot)
//    xyplot(mat4Plot -> (0 until mat4Plot.numCols - 1).map(x => line(yCol = x, xCol = mat4Plot.numCols - 1,
//      color = DiscreteColors(mat4Plot.numCols - 2)(x.toDouble))).toList)(
//      ylim = Option(min4Plot, max4Plot), xlim = Option(0.0, (mat4Plot.numRows - 1).toDouble))
//  }
//
//  def plotKSeqToPng(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
//                    fileName: String, widthPng: Int = 1000) = {
//    val filePng = new File(fileName)
//    pngToFile(filePng, plotSeq.build, widthPng)
//  }
//
//  def writeClusterSizeCsv(kMeanCluster: org.saddle.Vec[Int], fileName: String) = {
//    val clusterSize = kMeanCluster.toArray.groupBy(identity).map(x => (x._1, x._2.size))
//    val clusterFrame = Frame("Cluster" -> Vec(clusterSize.map(_._1 + 1).toArray), "Size" -> Vec(clusterSize.map(_._2).toArray))
//    // _._1 + 1 for starting cluster nb with 1
//    clusterFrame.writeCsvFile(fileName)
//  }
//}