
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
import shapeless.Inr
import stat.kmeans._
import stat.sparse.SMat

import scala.collection.parallel.mutable._

object ClusterEllipse {
  class EllipseException(listExceptionClId: List[EllipseClusterId]) extends Exception() {
    val sizeId = listExceptionClId.filter(clId => clId.cluster.size == 1).map(clId => (clId.cluster.size, clId.clusterId))
    val zeroVarId = listExceptionClId.filter(clId => ((clId.cluster.ellipseMat == null) && (clId.cluster.size > 1))).
      map(clId => (clId.cluster.zeroVarIndex, clId.clusterId))

    def errMessage(): String = {
      "Ellipse cluster exception: \n" +
        sizeId.map(x => "Size: " + x._1.toString + " of cluster " + (x._2 + 1).toString).mkString("\n") +
        " Zero variance of " +
        zeroVarId.map(x => "Indices " + (x._1).map(_ + 1).mkString(",") + " of cluster " + (x._2 + 1).toString).mkString("\n")
    }
  }

  case class EllipseCluster(size: Int, mean: Array[Double], varMat: DenseMatrix[Double], giveEllispeMat: DenseMatrix[Double] = null) {
    val ellipseMat = if (giveEllispeMat == null) {
      try (inv(varMat)) catch {
        case _: Throwable => {
          println("Impossible to invert matrix, may cause problems for ellipse/tree")
          println("Data size: " + size)
          println(varMat)
          null
        }
      }
    } else giveEllispeMat
    val zeroVarIndex = (0 until varMat.cols).filter(x => varMat(x, x) == 0)
  }

  case class EllipseClusterId(cluster: EllipseCluster, clusterId: Int) {
    def double2Hex(db : Double) : String = java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(db)
    var Name : String = clusterId.toString
    def toHexString(paramNames : Array[String]): String = {
      "Parameters=" + paramNames.mkString(":") + ";Name=" + Name + ";Size=" + cluster.size.toString +
        "\nMeans=" + cluster.mean.map(x => double2Hex(x)).mkString(":") +
        "\nvar=" + cluster.varMat.toArray.map(x => double2Hex(x)).mkString(":") +
        "\nellispe=" + cluster.varMat.toArray.map(x => double2Hex(x)).mkString(":") +
        "\n"
    }
  }

  def hexStringToElClusterId(hxString : String)

  def fusionEllipseCluster(clusterA: EllipseCluster, clusterB: EllipseCluster): EllipseCluster = {
    val sizeFus = clusterA.size + clusterB.size
    val meanFus = (clusterA.mean).zip(clusterB.mean).
      map(x => (x._1 * clusterA.size + x._2 * clusterB.size) / sizeFus)
    val sumX2A = (clusterA.varMat.map(x => x * (clusterA.size) / (clusterA.size - 1)) +
      (DenseMatrix(clusterA.mean).t) * DenseMatrix(clusterA.mean)).map(x => x * (clusterA.size))
    val sumX2B = (clusterB.varMat.map(x => x * (clusterB.size) / (clusterB.size - 1)) +
      (DenseMatrix(clusterB.mean).t) * DenseMatrix(clusterB.mean)).map(x => x * (clusterB.size))
    val varFus = ((sumX2A + sumX2B).
      map(x => x / (sizeFus)) - (DenseMatrix(meanFus).t) * DenseMatrix(meanFus)).map(x => x * sizeFus / (sizeFus - 1))
    EllipseCluster(sizeFus, meanFus, varFus)
  }

  def distEllipseCluster(point : Array[Double],ellipseCluster : EllipseCluster) : Double = {
    (DenseMatrix(point)*(ellipseCluster.ellipseMat)*(DenseMatrix(point).t)).apply(0,0)}

  def distEllipseCluster(clusterA: EllipseCluster, clusterB: EllipseCluster): Double = {
    val minVect = inv(clusterA.ellipseMat + clusterB.ellipseMat) *
      (clusterA.ellipseMat * (DenseMatrix(clusterA.mean).t) + clusterB.ellipseMat * (DenseMatrix(clusterB.mean).t))

    (((minVect - DenseMatrix(clusterA.mean).t).t) * clusterA.ellipseMat * (minVect - DenseMatrix(clusterA.mean).t) +
      ((minVect - DenseMatrix(clusterB.mean).t).t) * clusterB.ellipseMat * (minVect - DenseMatrix(clusterB.mean).t)).apply(0, 0)
  }

  case class ArrowEllipseCluster(source: EllipseClusterId, target: EllipseClusterId) {}

  //careful: the treeEllipseCluster create a new fusion cluster with a maxId above the max of Ids. This could overload the Ids
  // if the method is applied on a clusterCutList on which some cluster has been removed by another method
  def treeEllipseCluster(clusterCutList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val maxId = clusterCutList.map(x => x.clusterId).max
    val clusterCutListNoOne = clusterCutList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterCutListNoOne.length == 1) Nil
    else {
      val minDistList = (for (g <- clusterCutListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterCutListNoOne(g(0)).cluster, clusterCutListNoOne(g(1)).cluster)
          , clusterCutListNoOne(g(0)).clusterId, clusterCutListNoOne(g(1)).clusterId)
      }).toList
      val minDist = minDistList.map(x => x._1).min
      val removeClusterId = minDistList.filter(x => (x._1 == minDist)).map(x => (x._2, x._3)).head
      val clusterA = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._1)).head
      val clusterB = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._2)).head
      val newCluster = EllipseClusterId(fusionEllipseCluster(clusterA.cluster, clusterB.cluster), maxId + 1)
      println("Construct fusion cluster " + (maxId + 1 + 1) + " from " + (removeClusterId._1 + 1) +
        " and " + (removeClusterId._2 + 1) + ", distance:" + minDist)
      val newClusterList = newCluster ::
        (clusterCutListNoOne.filter(x => ((x.clusterId != removeClusterId._1) && (x.clusterId != removeClusterId._2))))
      ArrowEllipseCluster(clusterA, newCluster) :: ArrowEllipseCluster(clusterB, newCluster) ::
        treeEllipseCluster(newClusterList)
    }
  }

  // construct tree level by level, from initial cluster as leaves, not tested yet
  def levelTreeEllipseCluster(clusterCutList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val maxId = clusterCutList.map(x => x.clusterId).max
    val clusterCutListNoOne = clusterCutList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterCutListNoOne.length == 1) Nil
    else {
      val minDistList: List[(Double, Int, Int)] = (for (g <- clusterCutListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterCutListNoOne(g(0)).cluster, clusterCutListNoOne(g(1)).cluster)
          , clusterCutListNoOne(g(0)).clusterId, clusterCutListNoOne(g(1)).clusterId)
      }).toList

      //closure for one level step
      def levelStep(distClustId: List[(Double, Int, Int)]): List[ArrowEllipseCluster] = {
        if (distClustId.length == 0) Nil
        else {
          val minDist = distClustId.map(x => x._1).min
          val removeClusterId = minDistList.filter(x => (x._1 == minDist)).map(x => (x._2, x._3)).head
          val clusterA = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._1)).head
          val clusterB = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._2)).head
          val newCluster = EllipseClusterId(fusionEllipseCluster(clusterA.cluster, clusterB.cluster), maxId + 1)
          println("Construct fusion cluster " + (maxId + 1 + 1) + " from " + (removeClusterId._1 + 1) + " and "
            + (removeClusterId._2 + 1) + ", distance:" + minDist)
          if (distClustId.length == 3) {
            val remainClusterId = distClustId.flatMap(distIdId => List(distIdId._1, distIdId._2)).filter(id =>
              (id != removeClusterId._1 && id != removeClusterId._2)).head
            val clusterC = clusterCutListNoOne.filter(x => (x.clusterId == remainClusterId)).head
            List(ArrowEllipseCluster(clusterA, newCluster), ArrowEllipseCluster(clusterB, newCluster),
              ArrowEllipseCluster(clusterC, clusterC))
          } else {
            val newdistClustId = distClustId.filter(distIdId =>
              ((distIdId._2 != clusterA.clusterId) && (distIdId._2 != clusterB.clusterId) &&
                (distIdId._3 != clusterA.clusterId) && (distIdId._3 != clusterB.clusterId)))
            ArrowEllipseCluster(clusterA, newCluster) :: ArrowEllipseCluster(clusterB, newCluster) ::
              levelStep(newdistClustId)
          }
        }
      }

      val levelArrowCluster = levelStep(minDistList)
      val newClusterList = levelArrowCluster.map(_.target)
      levelArrowCluster ::: levelTreeEllipseCluster(newClusterList)
    }
  }

  def connectedCluster(clusterList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val clusterListNoOne = clusterList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterListNoOne.length == 1) Nil
    else {
      val distList: List[(Double, Int, Int)] = (for (g <- clusterListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterListNoOne(g(0)).cluster, clusterListNoOne(g(1)).cluster)
          , clusterListNoOne(g(0)).clusterId, clusterListNoOne(g(1)).clusterId)
      }).toList

      def recurConnClusterNet(cListComp: List[(EllipseClusterId, Int)], dList: List[(Double, Int, Int)]):
      List[ArrowEllipseCluster] = {
        if (cListComp.map(x => x._2).distinct.length == 1) Nil else {
          val connectDistList = dList.filter(dist => {
            val compA = cListComp.filter(cl => cl._1.clusterId == dist._2).head._2
            val compB = cListComp.filter(cl => cl._1.clusterId == dist._3).head._2
            compA != compB
          })
          val connectDist = connectDistList.filter(dist => (dist._1 == connectDistList.map(_._1).min)).head
          val clusterCompA = cListComp.filter(x => (x._1.clusterId == connectDist._2)).head
          val clusterCompB = cListComp.filter(x => (x._1.clusterId == connectDist._3)).head
          val newCListComp = cListComp.map(clComp => {
            if (clComp._2 == clusterCompB._2)
              (clComp._1,clusterCompA._2)
            else clComp
          })
          ArrowEllipseCluster(clusterCompA._1, clusterCompB._1) :: recurConnClusterNet(newCListComp, dList)
        }
      }
      recurConnClusterNet(clusterListNoOne.zipWithIndex, distList)
    }
  }
}