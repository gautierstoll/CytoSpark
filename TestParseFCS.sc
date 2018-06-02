
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

//import java.nio.ByteBuffer

//val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)
//
//val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,0))
//
//val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6,10000,100,50))
///
//val inputExp12 =
//  FCSInputFull("Exp 12 T cells_Tube_001.fcs",
//    List((5,true,0.0), (6,true,0.0), (7,true,0.0), (8,true,0.0), (9,true,0.0), (10,true,0.0), (11,true,0.0)),10000)
//
//val dataExp12 = new FCSParserFull(inputExp12)
//val kmeanExp12 = dataExp12.kmeansFCS(KMeanFCSInput(7,10000,100,0))
////
case class ellipseCluster(size: Int, mean: Array[Double], varMat: DenseMatrix[Double]) {}

def fusionEllipseCluster(clusterA: ellipseCluster, clusterB: ellipseCluster): ellipseCluster = {
  val sizeFus = clusterA.size + clusterB.size
  val meanFus = (clusterA.mean).zip(clusterB.mean).
    map(x => (x._1 * clusterA.size + x._2 * clusterB.size) / sizeFus)
  val sumX2A = (clusterA.varMat.map(x=>x*(clusterA.size)/(clusterA.size-1)) +
    (DenseMatrix(clusterA.mean).t)*DenseMatrix(clusterA.mean)).map(x=> x*(clusterA.size))
  val sumX2B = (clusterB.varMat.map(x=>x*(clusterB.size)/(clusterB.size-1)) +
    (DenseMatrix(clusterB.mean).t)*DenseMatrix(clusterB.mean)).map(x=> x*(clusterB.size))
  val varFus = ((sumX2A + sumX2B).map(x=>x/(sizeFus)) - (DenseMatrix(meanFus).t)*DenseMatrix(meanFus)).map(x=>x*sizeFus/(sizeFus-1))
  ellipseCluster(sizeFus,meanFus,varFus)
}

def minDist(clusterA: ellipseCluster,clusterB : ellipseCluster) : Double = {
  val matEllipseA = inv(clusterA.varMat)
  val matEllipseB = inv(clusterB.varMat)
  val minVect = inv(matEllipseA+matEllipseB)*
    (matEllipseA*DenseMatrix(clusterA.mean)+matEllipseB*DenseMatrix(clusterB.mean))
  ((minVect - DenseMatrix(clusterA.mean)).t)*matEllipseA*(minVect - DenseMatrix(clusterA.mean)) +
    ((minVect - DenseMatrix(clusterB.mean)).t)*matEllipseB*(minVect - DenseMatrix(clusterB.mean)) +
}
