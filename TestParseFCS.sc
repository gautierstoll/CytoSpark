
import java.io._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._
import java.nio.ByteBuffer

import ClusterEllipse._
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

import java.nio.ByteBuffer

//val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)
//
//val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,0))
//
//val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6,10000,100,50))
///
val inputExp12 =
  FCSInputFull("Exp 12 T cells_Tube_001.fcs",
    List((5,true,0.0), (6,true,0.0), (7,true,0.0), (8,true,0.0), (9,true,0.0), (10,true,0.0), (11,true,0.0)),1000)
//    List((5,false,0.0), (6,false,0.0), (7,false,0.0), (8,false,0.0), (9,false,0.0), (10,false,0.0), (11,false,0.0)),10000)


//val dataExp12 = new FCSParserFull(inputExp12)
//val kmeanExp12 = dataExp12.kmeansFCS(KMeanFCSInput(20,10000,100,10))
//def testFunction(testvar : Int) =
//{
//  var varList : List[Int] = null
//  if (varList == null) println("List is null")
//  testvar match {
//    case 3 => {
//      varList = testvar :: List(testvar+1)
//    }
//    case _:Int => {}
//  }
//  if (varList == null) println("List is still null")
//  else println(varList)
//}


//
//
//val dataA1 = Array(1d,3d,5d,10d)
//val dataA2 = Array(10d,33d,4d,11d)
//val meanA=Array(mean(dataA1),mean(dataA2))
//val covMatA=covmat(DenseMatrix(dataA1,dataA2).t)
//val clusterA = ClusterEllipse.EllipseClusterId(EllipseCluster(4,meanA,covMatA),0)
//
//val dataB1 = Array(2d,4d,10d)
//val dataB2 = Array(11d,23d,5d)
//val meanB=Array(mean(dataB1),mean(dataB2))
//val covMatB=covmat(DenseMatrix(dataB1,dataB2).t)
//val clusterB = ClusterEllipse.EllipseClusterId(EllipseCluster(3,meanA,covMatB),1)
//
//val dataC1 = Array(-1d,-3d,-8d,-10d,22d)
//val dataC2 = Array(80d,43d,40d,55d,66d)
//val meanC=Array(mean(dataC1),mean(dataC2))
//val covMatC=covmat(DenseMatrix(dataC1,dataC2).t)
//val clusterC = ClusterEllipse.EllipseClusterId(EllipseCluster(5,meanC,covMatC),2)
//
//val treeEllipse = ClusterEllipse.treeEllipseCluster(List(clusterA,clusterB,clusterC))