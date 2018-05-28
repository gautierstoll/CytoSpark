

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

//import java.nio.ByteBuffer

object Main extends App {
  val fcsFile = scala.io.StdIn.readLine("FCS File: ")
  val exp12FCS = new FCSParserCompact(fcsFile, -1000)

  val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6, 1000, 100, 0))

  val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6, 10000, 100, 50))
  println("Clusters: ")
  kMeanExp12_Big.clusters.toArray.groupBy(identity).map(x => (x._1, x._2.size)).foreach(println)
}