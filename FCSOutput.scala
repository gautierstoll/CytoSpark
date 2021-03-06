//
import java.io._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.{Files, Paths}

import ClusterEllipse._
import org.saddle._
import stat._

import scala.util._
import breeze.numerics.constants._
import org.nspl
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

object FCSOutput {

  //def clusterForPlot(fcsParsed: FCSParserFull, kMeanR: KMeansResult): (List[EllipseClusterId], Array[String]) = {
  /**
    *
    * @param fcsDataFinalKMean final KMean cluster and data
    * @return cluster and parameters names for 2d plot
    */
  def clusterForPlot(fcsDataFinalKMean: FCSDataFinalKMean): (List[EllipseClusterId], Array[String]) = {
    val clusterList = fcsDataFinalKMean.bestKMean.clusters.toSeq.distinct.toParArray.map(clusterId => {
      val indexId = fcsDataFinalKMean.bestKMean.clusters.toSeq.zipWithIndex.filter(x => x._1 == clusterId).map(_._2)
      val dataMat = fcsDataFinalKMean.dataMat.row(indexId.toArray)
      ClusterEllipse.EllipseClusterId(ClusterEllipse.EllipseCluster(indexId.length,
        dataMat.cols.map(x => breeze.stats.mean(x.toArray)).toArray,
        covmat(new DenseMatrix(dataMat.numCols, dataMat.numRows, dataMat.toArray).t)
      ), clusterId)
    }).toList
    val labelParam = fcsDataFinalKMean.takenParam.map(param =>
      try fcsDataFinalKMean.textSegmentMap("$P" + param + "S") catch {
        case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + param + "N")
      }
    ).toArray
    (clusterList, labelParam)
  }

  /** plot ellipse, from list of EllipseClusterId, minmax based on ellipses
    *
    * @param clusterListParam EllipseClusterrin
    * @param excludeCluster indices of cluster to exclude
    * @param excludeParam indices of paramters to exclude
    * @return
    */
  def kMeanFCSPlotEllipse2D(clusterListParam: ClusterEllipse.EllipseClustering,
                            excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    def ellipsePoints2DVar(cluster: EllipseCluster, index1: Int, index2: Int, segmentNb: Int): org.saddle.Mat[Double] = {
      def ellipse2DVarCurve(cluster: EllipseCluster, index1: Int, index2: Int): Double => (Double, Double) = {
        val svdCov = svd(cluster.varMat(List(index1, index2), List(index1, index2)).toDenseMatrix)
        val Vt = svdCov.Vt
        val S = svdCov.S
        angle => {
          val vectEllipse = Vt * DenseVector(pow(S(0), .5) * cos(angle), pow(S(1), .5) * sin(angle))
          (vectEllipse(0) + cluster.mean(index1), vectEllipse(1) + cluster.mean(index2))
        }
      }

      val ellipseD1D2 = ellipse2DVarCurve(cluster, index1, index2)
      Mat(segmentNb + 1, 2, (0 to segmentNb).map(x => x.toDouble / segmentNb * 2 * Pi).map(x => ellipseD1D2(x)).flatMap(x => Array(x._1, x._2)).toArray)
    }

    //clusterListParam._1.filter(eClId => (eClId.cluster.size < 2) ).foreach(eClId => println("Cluster "+(eClId.clusterId+1)+" has size 1"))
    val clusterListParam4Plot = (clusterListParam.listEllipse.filter(x => !excludeCluster.contains(x.clusterId)), clusterListParam.param)
    val errCluster = clusterListParam4Plot._1.
      filter(clId => (clId.cluster.size == 1) ||
        (clId.cluster.zeroVarIndex.filter(zeroVar => !excludeParam.contains(zeroVar)).length > 0))
    if (errCluster.nonEmpty) throw new ClusterEllipse.EllipseException(errCluster)

    val param4Plot = clusterListParam.param.indices.filter(x => !excludeParam.contains(x))
    val projections = param4Plot.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "     \r")
      val clusterEllipseMatForPlot = Mat(clusterListParam4Plot._1.flatMap(ellClustId =>
        ellipsePoints2DVar(ellClustId.cluster, c1, c2, 100).cols.toList :::
          List(Vec(Array.fill(100 + 1)(ellClustId.clusterId.toDouble)))).toArray)
      val indexX = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3).toArray
      val indexY = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3 + 1).toArray
      val xMinMax = Option(clusterEllipseMatForPlot.col(indexX).toArray.min - abs(clusterEllipseMatForPlot.col(indexX).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexX).toArray.max + abs(clusterEllipseMatForPlot.col(indexX).toArray.max) * .05)
      val yMinMax = Option(clusterEllipseMatForPlot.col(indexY).toArray.min - abs(clusterEllipseMatForPlot.col(indexY).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexY).toArray.max + abs(clusterEllipseMatForPlot.col(indexY).toArray.max) * .05)
      xyplot(clusterEllipseMatForPlot ->
        (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => line(xCol = x * 3, yCol = x * 3 + 1, colorCol = x * 3 + 2,
          color = DiscreteColors(clusterListParam.param.length - 1))).toList)(
        xlim = xMinMax, ylim = yMinMax,
        extraLegend = clusterListParam4Plot._1.map(_.clusterId).toArray.sorted.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(clusterListParam.listEllipse.length - 1)(x.toDouble))),
        xlab = clusterListParam.param(c1), ylab = clusterListParam.param(c2)
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  //  def kMeanFCSPlot2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, excludeCluster: Array[Int] = Array())
  /** 2D scatter plots
    *
    * @param fcsDataFinalKMean
    * @param excludeCluster
    * @param excludeParam
    * @return
    */
  def kMeanFCSPlot2D(fcsDataFinalKMean: FCSDataFinalKMean, excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until fcsDataFinalKMean.bestKMean.clusters.length).
      filter(x => !excludeCluster.contains(fcsDataFinalKMean.bestKMean.clusters(x).toArray.head)).toArray
    val dataSubFCS = fcsDataFinalKMean.dataMat.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = fcsDataFinalKMean.bestKMean.clusters.filter(x => !excludeCluster.contains(x)),
      means = fcsDataFinalKMean.bestKMean.means
    )
    val param4Plot = fcsDataFinalKMean.takenParam.indices.filter(x => !excludeParam.contains(x))
    val projections = param4Plot.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "        \r")
      val xMinMaxFCSComp = Option(dataSubFCS.col(c1).toArray.min, dataSubFCS.col(c1).toArray.max)
      val yMinMaxFCSComp = Option(dataSubFCS.col(c2).toArray.min, dataSubFCS.col(c2).toArray.max)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      xyplot(
        Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)) -> point(
          labelText = false, size = 4.0 / log10(fcsDataFinalKMean.bestKMean.clusters.length),
          color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.sorted.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)(x.toDouble))),
        xlab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "N")
        },
        ylab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  //def kMeanFCSPlot2DGrid(fcsParsed: FCSParserFull, kMeanR: KMeansResult, grid2D: Int, excludeCluster: Array[Int] = Array())
  /** 2D scatter Grid plots
    *
    * @param fcsDataFinalKMean
    * @param grid2D
    * @param excludeCluster
    * @param excludeParam
    * @return
    */
  def kMeanFCSPlot2DGrid(fcsDataFinalKMean: FCSDataFinalKMean, grid2D: Int, excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {

    val keepIndex = (0 until fcsDataFinalKMean.bestKMean.clusters.length).
      filter(x => !excludeCluster.contains(fcsDataFinalKMean.bestKMean.clusters(x).toArray.head)).toArray
    val dataSubFCS = fcsDataFinalKMean.dataMat.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = fcsDataFinalKMean.bestKMean.clusters.filter(x => !excludeCluster.contains(x)),
      means = fcsDataFinalKMean.bestKMean.means
    )
    val param4Plot = fcsDataFinalKMean.takenParam.indices.filter(x => !excludeParam.contains(x))
    val projections = param4Plot.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "        \r")
      val xMin = dataSubFCS.col(c1).toArray.min
      val xMax = dataSubFCS.col(c1).toArray.max
      val yMin = dataSubFCS.col(c2).toArray.min
      val yMax = dataSubFCS.col(c2).toArray.max
      val gridX = grid2D.toDouble / (xMax - xMin)
      val gridY = grid2D.toDouble / (yMax - yMin)
      val xMinMaxFCSComp = Option(xMin, xMax)
      val yMinMaxFCSComp = Option(yMin, yMax)
      val array2DCluster = dataSubFCS.col(c1).toArray.zip(dataSubFCS.col(c2).toArray).zip(subKMeanR.clusters.map(_.toDouble).toArray).
        map(x => (x._1._1, x._1._2, x._2))
      val gridGroupArray = array2DCluster.groupBy(x => floor((x._1 - xMin) * gridX) * grid2D + floor((x._2 - yMin) * gridY)).map(_._2.head)
      val mat4Plot = Mat(gridGroupArray.map(_._1).toArray, gridGroupArray.map(_._2).toArray, gridGroupArray.map(_._3).toArray)
      xyplot(
        mat4Plot -> point(
          labelText = false, size = 4.0 / log10(gridGroupArray.toArray.length),
          color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.sorted.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)(x.toDouble))),
        xlab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "N")
        },
        ylab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  //  def kMeanFCSPlotClusters2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, excludeCluster: Array[Int] = Array())
  /** 2d plots of kmean cluster centers
    *
    * @param fcsDataFinalKMean
    * @param excludeCluster
    * @param excludeParam
    * @return
    */
  def kMeanFCSPlotClusters2D(fcsDataFinalKMean: FCSDataFinalKMean, excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until fcsDataFinalKMean.bestKMean.clusters.length).
      filter(x => !excludeCluster.contains(fcsDataFinalKMean.bestKMean.clusters(x).toArray.head)).toArray
    val dataSubFCS = fcsDataFinalKMean.dataMat.row(keepIndex)
    val clusterSize = fcsDataFinalKMean.bestKMean.clusters.toArray.groupBy(x => x).map(x => (x._1, x._2.length)).
      filter(x => !excludeCluster.contains(x._1)).toList.sortBy(_._1)
    val clusterMean = fcsDataFinalKMean.bestKMean.means.zipWithIndex.filter(x => !excludeCluster.contains(x._2))
    val param4Plot = fcsDataFinalKMean.bestKMean.means.head.toArray.indices.filter(x => !excludeParam.contains(x))
    val projections = param4Plot.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "       " + "\r")
      val xMinMaxFCSComp = Option(dataSubFCS.col(c1).toArray.min, dataSubFCS.col(c1).toArray.max)
      val yMinMaxFCSComp = Option(dataSubFCS.col(c2).toArray.min, dataSubFCS.col(c2).toArray.max)
      // unormalized cluster centers
      val col1 = clusterMean.map(x => x._1.raw(c1)).toArray.
        map(x => x * fcsDataFinalKMean.sdCol(c1) + fcsDataFinalKMean.meanCol(c1))
      val col2 = clusterMean.map(x => x._1.raw(c2)).toArray.
        map(x => x * fcsDataFinalKMean.sdCol(c2) + fcsDataFinalKMean.meanCol(c2))
      val totalSize = clusterSize.map(_._2.toDouble).sum
      xyplot(
        Mat(Vec(col1), Vec(col2),
          Vec(clusterSize.map(x => x._1.toDouble).toArray),
          Vec(clusterSize.map(x => 10 * log10(x._2.toDouble) / log10(totalSize.toDouble)).toArray)) ->
          point(
            labelText = false,
            color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = clusterSize.toArray.map(
          x =>
            (x._1 + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1)(x._1.toDouble))),
        xlab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c1) + "N")
        },
        ylab = try fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "S") catch {
          case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + fcsDataFinalKMean.takenParam(c2) + "N")
        }
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  /** plot ellipse with centers
    *
    * @param fcsDataFinalKMean
    * @param clusterListParam
    * @param excludeCluster
    * @param excludeParam
    * @return
    */
  def kMeanFCSPlotClusterEllipse2D(fcsDataFinalKMean: FCSDataFinalKMean,clusterListParam: ClusterEllipse.EllipseClustering,
                            excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    def ellipsePoints2DVar(cluster: EllipseCluster, index1: Int, index2: Int, segmentNb: Int): org.saddle.Mat[Double] = {
      def ellipse2DVarCurve(cluster: EllipseCluster, index1: Int, index2: Int): Double => (Double, Double) = {
        val svdCov = svd(cluster.varMat(List(index1, index2), List(index1, index2)).toDenseMatrix)
        val Vt = svdCov.Vt
        val S = svdCov.S
        angle => {
          val vectEllipse = Vt * DenseVector(pow(S(0), .5) * cos(angle), pow(S(1), .5) * sin(angle))
          (vectEllipse(0) + cluster.mean(index1), vectEllipse(1) + cluster.mean(index2))
        }
      }

      val ellipseD1D2 = ellipse2DVarCurve(cluster, index1, index2)
      Mat(segmentNb + 1, 2, (0 to segmentNb).map(x => x.toDouble / segmentNb * 2 * Pi).map(x => ellipseD1D2(x)).flatMap(x => Array(x._1, x._2)).toArray)
    }
    // Ellipses
    //clusterListParam._1.filter(eClId => (eClId.cluster.size < 2) ).foreach(eClId => println("Cluster "+(eClId.clusterId+1)+" has size 1"))
    val clusterListParam4Plot = (clusterListParam.listEllipse.filter(x => !excludeCluster.contains(x.clusterId)), clusterListParam.param)
    val errCluster = clusterListParam4Plot._1.
      filter(clId => (clId.cluster.size == 1) ||
        clId.cluster.zeroVarIndex.filter(zeroVar => !excludeParam.contains(zeroVar)).nonEmpty)
    if (errCluster.nonEmpty) throw new ClusterEllipse.EllipseException(errCluster)
    // Centers
    val keepIndex = (0 until fcsDataFinalKMean.bestKMean.clusters.length).
      filter(x => !excludeCluster.contains(fcsDataFinalKMean.bestKMean.clusters(x).toArray.head)).toArray
    val dataSubFCS = fcsDataFinalKMean.dataMat.row(keepIndex)
    val clusterSize = fcsDataFinalKMean.bestKMean.clusters.toArray.groupBy(x => x).map(x => (x._1, x._2.length)).
      filter(x => !excludeCluster.contains(x._1)).toList.sortBy(_._1)
    val clusterMean = fcsDataFinalKMean.bestKMean.means.zipWithIndex.filter(x => !excludeCluster.contains(x._2))

    val param4Plot = clusterListParam.param.indices.filter(x => !excludeParam.contains(x))
    val projections = param4Plot.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "     \r")
      val clusterEllipseMatForPlot = Mat(clusterListParam4Plot._1.flatMap(ellClustId =>
        ellipsePoints2DVar(ellClustId.cluster, c1, c2, 100).cols.toList :::
          List(Vec(Array.fill(100 + 1)(ellClustId.clusterId.toDouble)))).toArray)
      val indexX = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3).toArray
      val indexY = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3 + 1).toArray
      val xMinMax = Option(clusterEllipseMatForPlot.col(indexX).toArray.min - abs(clusterEllipseMatForPlot.col(indexX).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexX).toArray.max + abs(clusterEllipseMatForPlot.col(indexX).toArray.max) * .05)
      val yMinMax = Option(clusterEllipseMatForPlot.col(indexY).toArray.min - abs(clusterEllipseMatForPlot.col(indexY).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexY).toArray.max + abs(clusterEllipseMatForPlot.col(indexY).toArray.max) * .05)
      val col1 = clusterMean.map(x => x._1.raw(c1)).toArray.
        map(x => x * fcsDataFinalKMean.sdCol(c1) + fcsDataFinalKMean.meanCol(c1))
      val col2 = clusterMean.map(x => x._1.raw(c2)).toArray.
        map(x => x * fcsDataFinalKMean.sdCol(c2) + fcsDataFinalKMean.meanCol(c2))
      val totalSize = clusterSize.map(_._2.toDouble).sum
      xyplot((clusterEllipseMatForPlot -> // ellipse
        (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => line(xCol = x * 3, yCol = x * 3 + 1, colorCol = x * 3 + 2,
          color = DiscreteColors(clusterListParam.listEllipse.length - 1))).toList),
        (Mat(Vec(col1), Vec(col2), // centers
          Vec(clusterSize.map(x => x._1.toDouble).toArray),
          Vec(clusterSize.map(x => 10 * log10(x._2.toDouble) / log10(totalSize.toDouble)).toArray)) ->
          point(
            labelText = false,
            color = DiscreteColors(fcsDataFinalKMean.bestKMean.means.length - 1))))( // centers
        xlim = xMinMax, ylim = yMinMax,
        extraLegend = clusterListParam4Plot._1.map(_.clusterId).toArray.sorted.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(clusterListParam.listEllipse.length - 1)(x.toDouble))),
        xlab = clusterListParam.param(c1), ylab = clusterListParam.param(c2)
      )
    }
    sequence(projections.toList, TableLayout(4))
  }


  def kMeanFCSPlotSeqEuclid(kmeanEuclid: ParArray[(List[Double], KMeansResult)]): org.nspl.Build[Elems2[XYPlotArea, Legend]]
  = {
    val min4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatten.min * .98
    val max4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatten.max * 1.02
    val mat4Plot = Mat((kmeanEuclid.map(_._1.toArray).toList :::
      List((0 until kmeanEuclid.map(_._1.toArray).toArray.head.length).toArray.map(_.toDouble))).toArray)
    xyplot(mat4Plot -> (0 until mat4Plot.numCols - 1).map(x => line(yCol = x, xCol = mat4Plot.numCols - 1,
      color = DiscreteColors(mat4Plot.numCols - 2)(x.toDouble))).toList)(
      ylim = Option(min4Plot, max4Plot), xlim = Option(0.0, (mat4Plot.numRows - 1).toDouble))
  }

  def treeKmeanClust(clusterListParam: EllipseClustering, excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array()):
  List[ClusterEllipse.ArrowEllipseCluster] = {

    val errCluster = clusterListParam.listEllipse.
      filter(clId => !excludeCluster.contains(clId.clusterId) && (
        (clId.cluster.size == 1) ||
          clId.cluster.zeroVarIndex.filter(zeroVar => !excludeParam.contains(zeroVar)).nonEmpty))
    if (errCluster.nonEmpty) throw new ClusterEllipse.EllipseException(errCluster)
    val keepParam = (0 until clusterListParam.listEllipse.head.cluster.mean.length).filter(i => !excludeParam.contains(i))
    val clusterList4Tree = clusterListParam.listEllipse.filter(clId => !excludeCluster.contains(clId.clusterId)).
      map(clId => EllipseClusterId(ClusterEllipse.EllipseCluster(clId.cluster.size,
        clId.cluster.mean.zipWithIndex.filter(x => keepParam.contains(x._2)).map(_._1),
        clId.cluster.varMat(keepParam, keepParam).toDenseMatrix), clId.clusterId))
    ClusterEllipse.treeEllipseCluster(clusterList4Tree)
  }

  def connNetworkClust(clusterListParam: EllipseClustering, excludeCluster: Array[Int] = Array(), excludeParam: Array[Int] = Array()):
  List[ClusterEllipse.ArrowEllipseCluster] = {

    val errCluster = clusterListParam.listEllipse.
      filter(clId => !excludeCluster.contains(clId.clusterId) && (
        (clId.cluster.size == 1) ||
          (clId.cluster.zeroVarIndex.filter(zeroVar => !excludeParam.contains(zeroVar)).length > 0)))
    if (errCluster.nonEmpty) throw new ClusterEllipse.EllipseException(errCluster)
    val keepParam = (0 until clusterListParam.listEllipse.head.cluster.mean.length).filter(i => !excludeParam.contains(i))
    val clusterList4Tree = clusterListParam.listEllipse.filter(clId => !excludeCluster.contains(clId.clusterId)).
      map(clId => EllipseClusterId(ClusterEllipse.EllipseCluster(clId.cluster.size,
        clId.cluster.mean.zipWithIndex.filter(x => keepParam.contains(x._2)).map(_._1),
        clId.cluster.varMat(keepParam, keepParam).toDenseMatrix), clId.clusterId))
    ClusterEllipse.connectedCluster(clusterList4Tree)
  }

  def networkKmeanClustPlot2D(paramName: Array[String], // length of paramName = length of cluster means, parameter coudl be excluded in treeKmeanClust
                              treeArrow: List[ClusterEllipse.ArrowEllipseCluster], excludeParam: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val matArrow4Plot = Mat(treeArrow.length, paramName.length * 2 + 1,
      treeArrow.
        flatMap(x => Array(x.source.cluster.mean, x.target.cluster.mean, Array(x.source.clusterId.toDouble + 1))).flatten.
        toArray)
    val distinctCluster = treeArrow.flatMap(x => List(x.source, x.target)).distinct
    val matCluster4Plot = Mat(distinctCluster.length, paramName.length + 1,
      distinctCluster.flatMap(x => Array(x.cluster.mean, Array(x.clusterId.toDouble + 1))).flatten.toArray)
    val projections = paramName.indices.combinations(2).map { g =>
      val cx = g(0)
      val cy = g(1)
      val c2x = g(0) + paramName.length
      val c2y = g(1) + paramName.length
      val overCol = paramName.length * 2 + 1
      print(cx + " x " + cy + "       " + "\r")
      val xMinMaxFCSComp: Option[(Double, Double)] = Option(matCluster4Plot.col(Array(cx)).toArray.min - abs(matCluster4Plot.col(Array(cx)).toArray.min) * .05,
        matCluster4Plot.col(Array(cx)).toArray.max + abs(matCluster4Plot.col(Array(cx)).toArray.max) * .05)
      val yMinMaxFCSComp = Option(matCluster4Plot.col(Array(cy)).toArray.min - abs(matCluster4Plot.col(Array(cy)).toArray.min) * .05,
        matCluster4Plot.col(Array(cy)).toArray.max + abs(matCluster4Plot.col(Array(cy)).toArray.max) * .05)
      xyplot((matArrow4Plot ->
        lineSegment(xCol = cx, yCol = cy, x2Col = c2x, y2Col = c2y, colorCol = overCol, stroke = Stroke(.3))),
        (matCluster4Plot -> point(xCol = cx, yCol = cy, colorCol = (paramName.length),
          sizeCol = overCol, shapeCol = overCol, errorTopCol = overCol, errorBottomCol = overCol,
          valueText = true, color = Color.BLACK, labelFontSize = 1 fts)
          ))(xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        xlab = paramName(g(0)),
        ylab = paramName(g(1))
      )
    }
    sequence(projections.toList, TableLayout(4))
  }


  def plotKSeqToPng(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
                    fileName: String, widthPng: Int = 1000): File = {
    val filePng = new File(fileName)
    pngToFile(filePng, plotSeq.build, widthPng)
  }

  def plotKSeqToPdf(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
                    fileName: String) : File = {
    val filePdf = new File(fileName)
    pdfToFile(filePdf, plotSeq.build)
  }


  def writeClusterSizeCsv(kMeanCluster: org.saddle.Vec[Int], fileName: String, clusterINames: Array[String] = null) : Unit = {
    val clusterSize = kMeanCluster.toSeq.groupBy(identity).map(x => (x._1, x._2.size)).toList.sortWith(_._1 < _._1)
    val clusterNames: Array[String] = if (clusterINames == null) {
      clusterSize.map(cl => {
        val nm = scala.io.StdIn.readLine("Name of cluster " + (cl._1+1) + " :")
        if (nm == "") (cl._1+1).toString else nm
      }).toArray
    } else clusterINames
    val clusterFrame = Frame("Cluster" -> Vec(clusterSize.map(_._1 + 1).toArray.map(_.toString)),
      "Size" -> Vec(clusterSize.map(_._2).toArray.map(_.toString)), "Name" -> Vec(clusterNames))
    clusterFrame.writeCsvFile(fileName)
  }

  def writeClusterTreeSizeCsv(treeClust: List[ClusterEllipse.ArrowEllipseCluster], fileName: String) : Unit = {
    val clusterSize = treeClust.map(arrow => arrow.source.cluster.size).toArray
    val clusterId = treeClust.map(arrow => arrow.source.clusterId + 1).toArray // +1 because cluster nb start at 1
    val clusterTarget = treeClust.map(arrow => arrow.target.clusterId + 1).toArray // +1 because cluster nb start at 1
    val treeFrame =
      Frame("Cluster" -> Vec(clusterId), "Size" -> Vec(clusterSize), "Target" -> Vec(clusterTarget))
    treeFrame.writeCsvFile(fileName)
  }
}
