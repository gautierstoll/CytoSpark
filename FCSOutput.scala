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


// methods for output
object FCSOutput {
  // cluster and parameters names for 2d plot
  def clusterForPlot(fcsParsed: FCSParserFull, kMeanR: KMeansResult): (List[EllipseClusterId], Array[String]) = {
    val clusterList = kMeanR.clusters.toSeq.distinct.toParArray.map(clusterId => {
      val indexId = kMeanR.clusters.toSeq.zipWithIndex.filter(x => x._1 == clusterId).map(_._2)
      val dataMat = fcsParsed.dataTakenMatFCS.row(indexId.toArray)
      ClusterEllipse.EllipseClusterId(ClusterEllipse.EllipseCluster(indexId.length,
        dataMat.cols.map(x => breeze.stats.mean(x.toArray)).toArray,
        covmat(new DenseMatrix(dataMat.numCols, dataMat.numRows, dataMat.toArray).t)
      ), clusterId)
    }).toList
    val labelParam = fcsParsed.takenParam.map(param =>
      try fcsParsed.fcsTextSegmentMap("$P" + param + "S") catch {
        case _: Throwable => fcsParsed.fcsTextSegmentMap("$P" + param + "N")
      }
    ).toArray
    (clusterList, labelParam)
  }

  // plot ellipse, from list of EllipseClusterId, minmax based on ellipses
  def kMeanFCSPlotEllipse2D(clusterListParam: (List[EllipseClusterId], Array[String]), excludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    def ellipsePoints2DVar(cluster: EllipseCluster, index1: Int, index2: Int, segmentNb: Int): org.saddle.Mat[Double] = {
      def ellipse2DVarCurve(cluster: EllipseCluster, index1: Int, index2: Int): (Double => (Double, Double)) = {
        val svdCov = svd(cluster.varMat(List(index1, index2), List(index1, index2)).toDenseMatrix)
        val Vt = svdCov.Vt
        val S = svdCov.S
        (angle => {
          val vectEllipse = Vt * DenseVector(pow(S(0), .5) * cos(angle), pow(S(1), .5) * sin(angle))
          (vectEllipse(0) + cluster.mean(index1), vectEllipse(1) + cluster.mean(index2))
        })
      }

      val ellipseD1D2 = ellipse2DVarCurve(cluster, index1, index2)
      Mat(segmentNb + 1, 2, (0 to (segmentNb)).map(x => x.toDouble / segmentNb * 2 * Pi).map(x => ellipseD1D2(x)).flatMap(x => Array(x._1, x._2)).toArray)
    }

    val clusterListParam4Plot = (clusterListParam._1.filter(x => !excludeCluster.contains(x.clusterId)), clusterListParam._2)
    val projections = clusterListParam._2.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "     \r")
      val clusterEllipseMatForPlot = Mat(clusterListParam4Plot._1.flatMap(ellClustId =>
        (ellipsePoints2DVar(ellClustId.cluster, c1, c2, 100).cols.toList :::
          List(Vec(Array.fill(100 + 1)(ellClustId.clusterId.toDouble))))).toArray)
      val indexX = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3).toArray
      val indexY = (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => x * 3 + 1).toArray
      val xMinMax = Option(clusterEllipseMatForPlot.col(indexX).toArray.min - abs(clusterEllipseMatForPlot.col(indexX).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexX).toArray.max + abs(clusterEllipseMatForPlot.col(indexX).toArray.max) * .05)
      val yMinMax = Option(clusterEllipseMatForPlot.col(indexY).toArray.min - abs(clusterEllipseMatForPlot.col(indexY).toArray.min) * .05,
        clusterEllipseMatForPlot.col(indexY).toArray.max + abs(clusterEllipseMatForPlot.col(indexY).toArray.max) * .05)
      xyplot(clusterEllipseMatForPlot ->
        (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => line(xCol = x * 3, yCol = x * 3 + 1, colorCol = x * 3 + 2,
          color = DiscreteColors(clusterListParam._1.length - 1))).toList)(
        xlim = xMinMax, ylim = yMinMax,
        extraLegend = clusterListParam4Plot._1.map(_.clusterId).toArray.sorted.map(
          x =>
            (x + 1).toString -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), //x._1 + 1 for starting cluster nb with 1
              color = DiscreteColors(clusterListParam._1.length - 1)(x.toDouble))),
        xlab = clusterListParam._2(c1), ylab = clusterListParam._2(c2)
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  // 2D scatter plots
  def kMeanFCSPlot2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, excludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!excludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(excludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = fcsParsed.takenParam.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "        \r")
      val xMinMaxFCSComp = Option(dataSubFCS.col(c1).toArray.min, dataSubFCS.col(c1).toArray.max)
      val yMinMaxFCSComp = Option(dataSubFCS.col(c2).toArray.min, dataSubFCS.col(c2).toArray.max)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      xyplot(
        Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)) -> point(
          labelText = false, size = 4.0 / log10(kMeanR.clusters.length),
          color = DiscreteColors(kMeanR.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.sorted.map(
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

  // 2D scatter Grid[ plots
  def kMeanFCSPlot2DGrid(fcsParsed: FCSParserFull, kMeanR: KMeansResult, grid2D: Int, excludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {

    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!excludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(excludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = fcsParsed.takenParam.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "        \r")
      val xMin = dataSubFCS.col(c1).toArray.min
      val xMax = dataSubFCS.col(c1).toArray.max
      val yMin = dataSubFCS.col(c2).toArray.min
      val yMax = dataSubFCS.col(c2).toArray.max
      val xMinMaxFCSComp = Option(xMin, xMax)
      val yMinMaxFCSComp = Option(yMin, yMax)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      val gridIndex = (for (gridX <- (0 until grid2D); gridY <- (0 until grid2D)) yield {
        col1.toArray.zip(col2.toArray).zipWithIndex.filter(
          x => ((x._1._1 >= (xMin + gridX * (xMax - xMin) / grid2D)) && (x._1._1 <= (xMin + (gridX + 1) * (xMax - xMin) / grid2D)) &&
            (x._1._2 >= (yMin + gridX * (yMax - yMin) / grid2D)) && (x._1._2 <= (yMin + (gridX + 1) * (yMax - yMin) / grid2D)))
        ).head._2
      }).toArray
      val mat4Plot = Mat(col1, col2, subKMeanR.clusters.map(_.toDouble)).row(gridIndex)
      xyplot(
        mat4Plot -> point(
          labelText = false, size = 4.0 / log10(kMeanR.clusters.length),
          color = DiscreteColors(kMeanR.means.length - 1)))(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.sorted.map(
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

  //2d ellipse plots, based on KMeanResults, minmax from data

  def kMeanFCSPlotEllipseFromData2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, excludeCluster: Array[Int] = Array())
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
      filter(x => (!excludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val subKMeanR = KMeansResult(
      clusters = kMeanR.clusters.filter(x => !(excludeCluster.contains(x))),
      means = kMeanR.means
    )
    val projections = fcsParsed.takenParam.indices.combinations(2).map { g =>
      val c1 = g(0)
      val c2 = g(1)
      print(c1 + " x " + c2 + "     \r")
      val xMinMaxFCSComp = Option(dataSubFCS.col(c1).toArray.min, dataSubFCS.col(c1).toArray.max)
      val yMinMaxFCSComp = Option(dataSubFCS.col(c2).toArray.min, dataSubFCS.col(c2).toArray.max)
      val col1 = dataSubFCS.col(c1)
      val col2 = dataSubFCS.col(c2)
      // full matrix, with x and y points for ellipse, with color
      val clusterEllipseMatForPlot = Mat(subKMeanR.clusters.toSeq.distinct.flatMap(clusterIndex =>
        (ellipsePoints2DVar(col1.toSeq.zip(subKMeanR.clusters.toSeq).filter(x => (x._2 == clusterIndex)).map(x => x._1).toArray,
          col2.toSeq.zip(subKMeanR.clusters.toSeq).filter(x => (x._2 == clusterIndex)).map(x => x._1).toArray, 100).cols.toList :::
          List(Vec(Array.fill(100 + 1)(clusterIndex.toDouble))))).toArray)
      xyplot(clusterEllipseMatForPlot ->
        (0 until (clusterEllipseMatForPlot.numCols / 3)).map(x => line(xCol = x * 3, yCol = x * 3 + 1, colorCol = x * 3 + 2,
          color = DiscreteColors(kMeanR.means.length - 1))).toList)(
        xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        extraLegend = subKMeanR.clusters.toArray.distinct.sorted.map(
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
  def kMeanFCSPlotClusters2D(fcsParsed: FCSParserFull, kMeanR: KMeansResult, excludeCluster: Array[Int] = Array())
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val keepIndex = (0 until kMeanR.clusters.length).
      filter(x => (!excludeCluster.contains(kMeanR.clusters(x).toArray.head))).toArray
    val dataSubFCS = fcsParsed.dataTakenMatFCS.row(keepIndex)
    val clusterSize = kMeanR.clusters.toArray.groupBy(x => x).map(x => (x._1, x._2.length)).
      filter(x => (!excludeCluster.contains(x._2))).toList.sortBy(_._1)
    val clusterMean = kMeanR.means.zipWithIndex.filter(x => (!excludeCluster.contains(x._2)))
    val projections = kMeanR.means.head.toArray.indices.combinations(2).map { g =>
      val c1 = (g(0))
      val c2 = (g(1))
      print(c1 + " x " + c2 + "       " + "\r")
      val xMinMaxFCSComp = Option(dataSubFCS.col(c1).toArray.min, dataSubFCS.col(c1).toArray.max)
      val yMinMaxFCSComp = Option(dataSubFCS.col(c2).toArray.min, dataSubFCS.col(c2).toArray.max)
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
    val min4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).min * .98
    val max4Plot = kmeanEuclid.map(_._1.toArray).toArray.flatMap(x => x).max * 1.02
    val mat4Plot = Mat((kmeanEuclid.map(_._1.toArray).toList :::
      List((0 until kmeanEuclid.map(_._1.toArray).toArray.head.length).toArray.map(_.toDouble))).toArray)
    xyplot(mat4Plot -> (0 until mat4Plot.numCols - 1).map(x => line(yCol = x, xCol = mat4Plot.numCols - 1,
      color = DiscreteColors(mat4Plot.numCols - 2)(x.toDouble))).toList)(
      ylim = Option(min4Plot, max4Plot), xlim = Option(0.0, (mat4Plot.numRows - 1).toDouble))
  }

  def treeKmeanClust(clusterListParam: (List[EllipseClusterId], Array[String]), excludeCluster: Array[Int] = Array()):
  List[ClusterEllipse.ArrowEllipseCluster] = {
    val clusterList4Tree = clusterListParam._1.filter(cluster => !excludeCluster.contains(cluster.clusterId))
    ClusterEllipse.treeEllipseCluster(clusterList4Tree)
  }

  //
  //
  //  def treeKmeanClust(fcsParsed: FCSParserFull, kMeanR: KMeansResult): List[ClusterEllipse.ArrowEllipseCluster] = {
  //    val clusterList = kMeanR.clusters.toSeq.distinct.map(clusterId => {
  //      val indexId = kMeanR.clusters.toSeq.zipWithIndex.filter(x => x._1 == clusterId).map(_._2)
  //      val dataMat = fcsParsed.dataTakenMatFCS.row(indexId.toArray)
  //      ClusterEllipse.EllipseClusterId(ClusterEllipse.EllipseCluster(indexId.length,
  //        dataMat.cols.map(x => breeze.stats.mean(x.toArray)).toArray,
  //        covmat(new DenseMatrix(dataMat.numCols, dataMat.numRows, dataMat.toArray).t)
  //      ), clusterId)
  //    }).toList
  //    ClusterEllipse.treeEllipseCluster(clusterList)
  //  }

  def treeKmeanClustPlot2D(clusterListParam: (List[EllipseClusterId], Array[String]), treeArrow: List[ClusterEllipse.ArrowEllipseCluster])
  : Build[ElemList[Elems2[XYPlotArea, Legend]]] = {
    val mat4Plot = Mat(treeArrow.length, (clusterListParam._2.length) * 2 + 1,
      treeArrow.
        flatMap(x => Array(x.source.cluster.mean, x.target.cluster.mean, Array(x.source.clusterId.toDouble + 1))).flatMap(x => x).
        toArray)
    val projections = clusterListParam._2.indices.combinations(2).map { g =>
      val cx = (g(0))
      val cy = (g(1))
      val c2x = g(0) + clusterListParam._2.length
      val c2y = g(1) + clusterListParam._2.length
      val overCol = (clusterListParam._2.length) * 2 + 1
      print(cx + " x " + cy + "       " + "\r")
      val xMinMaxFCSComp = Option(mat4Plot.col(Array(cx, c2x)).toArray.min - abs(mat4Plot.col(Array(cx, c2x)).toArray.min) * .05,
        mat4Plot.col(Array(cx, c2x)).toArray.max + abs(mat4Plot.col(Array(cx, c2x)).toArray.max) * .05)
      val yMinMaxFCSComp = Option(mat4Plot.col(Array(cy, c2y)).toArray.min - abs(mat4Plot.col(Array(cy, c2y)).toArray.min) * .05,
        mat4Plot.col(Array(cy, c2y)).toArray.max + abs(mat4Plot.col(Array(cy, c2y)).toArray.max) * .05)
      xyplot(mat4Plot ->
        List(lineSegment(xCol = cx, yCol = cy, x2Col = c2x, y2Col = c2y, colorCol = overCol, stroke = Stroke(.5)),
          point(xCol = cx, yCol = cy, colorCol = (clusterListParam._2.length) * 2,
            sizeCol = overCol, shapeCol = overCol, errorTopCol = overCol, errorBottomCol = overCol,
            valueText = true, color = Color.BLACK, labelFontSize =.6 fts)
        ))(xlim = xMinMaxFCSComp, ylim = yMinMaxFCSComp,
        xlab = clusterListParam._2(g(0)),
        ylab = clusterListParam._2(g(1))
      )
    }
    sequence(projections.toList, TableLayout(4))
  }

  def plotKSeqToPng(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
                    fileName: String, widthPng: Int = 1000) = {
    val filePng = new File(fileName)
    pngToFile(filePng, plotSeq.build, widthPng)
  }

  def plotKSeqToPdf(plotSeq: Build[ElemList[Elems2[org.nspl.XYPlotArea, org.nspl.Legend]]],
                    fileName: String) = {
    val filePdf = new File(fileName)
    pdfToFile(filePdf, plotSeq.build)
  }


  def writeClusterSizeCsv(kMeanCluster: org.saddle.Vec[Int], fileName: String) = {
    val clusterSize = kMeanCluster.toArray.groupBy(identity).map(x => (x._1, x._2.size))
    val clusterFrame = Frame("Cluster" -> Vec(clusterSize.map(_._1 + 1).toArray), "Size" -> Vec(clusterSize.map(_._2).toArray))
    clusterFrame.writeCsvFile(fileName)
  }

  def writeClusterTreeSizeCsv(treeClust: List[ClusterEllipse.ArrowEllipseCluster], fileName: String) = {
    val clusterSize = treeClust.map(arrow => arrow.source.cluster.size).toArray
    val clusterId = treeClust.map(arrow => arrow.source.clusterId + 1).toArray // +1 because cluster nb start at 1
    val clusterTarget = treeClust.map(arrow => arrow.target.clusterId + 1).toArray // +1 because cluster nb start at 1
    val treeFrame = Frame("Cluster" -> Vec(clusterId), "Size" -> Vec(clusterSize), "Target" -> Vec(clusterTarget))
    treeFrame.writeCsvFile(fileName)
  }
}
