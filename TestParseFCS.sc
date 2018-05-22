import java.nio._

import Array._
import breeze.linalg._
import breeze.numerics._
import org.nspl.Stroke
import org.saddle._
//import java.nio.ByteBuffer

  def kMeanFCSPlotClustersConv(clusterConv: Array[List[IndexedSeq[Vec[Double]]]]) =

    (0 to (clusterConv.head.head.head.length - 1)).map(paramComp => {
      ((for (runIndex <- (0 to (clusterConv.length - 1));
                                  clusterIndex <- (0 to (clusterConv.head.head.length - 1))) yield {
        (for (convIndex <- (0 to (clusterConv.head.length -1))) yield
          {
            clusterConv(runIndex).toArray.
              zipWithIndex.filter(_._2 == convIndex).map(_._1).head.
              zipWithIndex.filter(_._2 == clusterIndex).map(_._1).head.toSeq.
              zipWithIndex.filter(_._2 == paramComp).map(_._1).head}).toArray}).toList :::
        List((0 to (clusterConv.head.length -1)).map(_.toDouble).toArray)).toArray})