import java.nio._

import Array._
import breeze.linalg._
import breeze.numerics._
import org.nspl._
import org.nspl.saddle._
import org.saddle._
import org.nspl.data._
import org.nspl.awtrenderer._
//import java.nio.ByteBuffer


val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)
val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,10,0))
val testConv = exp12FCS.kmeansCompensatedTestConv(KMeanFCSInput(6,10000,10,10),7,Array(0,10,30))

def matrixClustersConv(clusterConv: Array[List[IndexedSeq[Vec[Double]]]]) = {
  (0 to (clusterConv.head.head.head.length - 1)).map(paramComp => {
    Mat(((for (runIndex <- (0 to (clusterConv.length - 1));
                                 clusterIndex <- (0 to (clusterConv.head.head.length - 1))) yield {
      (for (convIndex <- (0 to (clusterConv.head.length -1))) yield
        {
          clusterConv(runIndex).toArray.
            zipWithIndex.filter(_._2 == convIndex).map(_._1).head.
            zipWithIndex.filter(_._2 == clusterIndex).map(_._1).head.toArray.
            zipWithIndex.filter(_._2 == paramComp).map(_._1).head}).toArray}).toList :::
      List((0 to (clusterConv.head.length -1)).map(_.toDouble).toArray)).toArray)
  })
}

