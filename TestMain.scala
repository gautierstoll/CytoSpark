

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
  val minCyt = scala.io.StdIn.readLine("Minimum compensated value: ").toDouble
  val exp12FCS = new FCSParserCompact(fcsFile, minCyt)
  println("Clustering paramters:")
  val nbCluster = scala.io.StdIn.readLine("Number of clusters [6]: ").
    map( x => if (x == "") 6 else x.toInt )
  val nbRow = scala.io.StdIn.readLine("Number of used rows ["+exp12FCS.nbEvent+"]: ").
    map( x => if (x == "") exp12FCS.nbEvent else x.toInt )
  val nbIteration : Int = scala.io.StdIn.readLine("Number of K-Mean iterations [100]: ").
    map( x => if (x == "") 100 else x.toInt )
  val nbStep : Int = scala.io.StdIn.readLine("Number of K-Meansteps [10]: ").
    map( x => if (x == "") 10 else x.toInt )
  val nbAttemps : Int = scala.io.StdIn.readLine("Number of K-Mean clustering [5]: ").
    map( x => if (x == "") 5 else x.toInt )
  val seed : Int = scala.io.StdIn.readLine("Pseudo-random generator inital condition [10]: ").
    map( x => if (x == "") 10 else x.toInt )
  // generate randome seed


  val kMeanEuclid = exp12FCS.kmeansCompensatedEuclidConv(KMeanFCSInput(nbCluster,nbRow,nbIteration,0),)



  val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6, 1000, 100, 0))

  val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6, 10000, 100, 50))
  println("Clusters: ")
  kMeanExp12_Big.clusters.toArray.groupBy(identity).map(x => (x._1+1, x._2.size)).foreach(println)
}