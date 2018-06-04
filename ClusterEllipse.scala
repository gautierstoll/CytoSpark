
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
import scala.collection.parallel.mutable._


object ClusterEllipse {

  case class EllipseCluster(size: Int, mean: Array[Double], varMat: DenseMatrix[Double]) {
    val ellipseMat = inv(varMat)
  }

  case class EllipseClusterId(cluster: EllipseCluster, clusterId: Int) {}

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

  def distEllipseCluster(clusterA: EllipseCluster, clusterB: EllipseCluster): Double = {
    val minVect = inv(clusterA.ellipseMat + clusterB.ellipseMat) *
      (clusterA.ellipseMat * DenseMatrix(clusterA.mean).t + clusterB.ellipseMat * DenseMatrix(clusterB.mean).t)

    (((minVect - DenseMatrix(clusterA.mean).t).t) * clusterA.ellipseMat * (minVect - DenseMatrix(clusterA.mean).t) +
      ((minVect - DenseMatrix(clusterB.mean).t).t) * clusterB.ellipseMat * (minVect - DenseMatrix(clusterB.mean).t)).apply(0, 0)
  }

  case class ArrowEllipseCluster(source: EllipseClusterId, target: EllipseClusterId) {}
//carfull: the treeEllipseCluster create a new fusion cluster with a maxId above the max of Ids. This could overload the Ids
  // if the method is applied on a clusterCutList on which some cluster has been removed by another method
  def treeEllipseCluster(clusterCutList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val maxId = clusterCutList.map(x=>x.clusterId).max
    if (clusterCutList.length == 1) Nil
    else {
      val minDistList = (for (g <- clusterCutList.indices.combinations(2)) yield {
        (distEllipseCluster(clusterCutList(g(0)).cluster, clusterCutList(g(0)).cluster)
          , clusterCutList(g(0)).clusterId, clusterCutList(g(1)).clusterId)
      }).toList
      val minDist = minDistList.map(x => x._1).min
      val removeClusterId = minDistList.filter(x => (x._1 == minDist)).map(x => (x._2, x._3)).head
      val clusterA = clusterCutList.filter(x => (x.clusterId == removeClusterId._1)).head
      val clusterB = clusterCutList.filter(x => (x.clusterId == removeClusterId._2)).head
      val newCluster = EllipseClusterId(fusionEllipseCluster(clusterA.cluster, clusterB.cluster), maxId + 1)
      val newClusterList = newCluster ::
        (clusterCutList.filter(x => ((x.clusterId != removeClusterId._1) && (x.clusterId != removeClusterId._2))))
      ArrowEllipseCluster(clusterA, newCluster) :: ArrowEllipseCluster(clusterB, newCluster) ::
        treeEllipseCluster(newClusterList)
    }
  }
}