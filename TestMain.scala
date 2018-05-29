

import java.io._

import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer

import org.nspl.awtrenderer._

import scala.collection.parallel.mutable._
import org.saddle._
import stat._
import java.nio.file.{Files, Paths}

import scala.util._
import org.nspl._
import org.nspl.saddle._
import org.nspl.data._
import org.nspl.awtrenderer._
import org.saddle.io._
import org.spark_project.dmg.pmml.{False, True}
import stat.kmeans._
import stat.sparse.SMat

import scala.collection.parallel.mutable.ParArray

//import java.nio.ByteBuffer

object Main extends App {
  val maxSeedParralel = 10000

  def takeIntFromLine(askingPromp: String, defaultVal: Int, minVal: Int): Int = {
    (scala.io.StdIn.readLine(askingPromp) match {
      case "" => defaultVal
      case x: String => try {
        x.toInt
      } catch {
        case _: Throwable => println("Take default " + defaultVal); defaultVal
      }
    })
    match {
      case y: Int => if (y < minVal) {
        println("Take min " + minVal);
        minVal
      }
      else y
    }
  }

  //def main() {
  val fcsFile = scala.io.StdIn.readLine("FCS File: ")
  if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " do not exist")

  val minCyt =
    scala.io.StdIn.readLine("Minimum compensated value[-1000]: ") match {
      case "" => -1000.0
      case x: String => try {
        x.toDouble
      } catch {
        case _: Throwable => println("Take default -1000"); -1000.0
      }
    }

  val exp12FCS = new FCSParserCompact(fcsFile, minCyt)
  var clusterLoop: Boolean = true
  while (clusterLoop) {
    println("Clustering parameters:")
    val nbCluster: Int = takeIntFromLine("Number of clusters [6]: ", 6, 1)
    val nbRow: Int = takeIntFromLine("Number of used rows [" + exp12FCS.nbEvent + "]: ", exp12FCS.nbEvent, 1)
    val nbIteration: Int = takeIntFromLine("Number of K-Mean iterations [100]: ", 100, 1)
    val nbStep: Int = takeIntFromLine("Number of K-Mean steps [5]: ", 5, 2)
    val nbAttemp: Int = takeIntFromLine("Number of K-Mean clustering [5]: ", 5, 1)
    val seed: Int = takeIntFromLine("Pseudo-random generator initial condition [10]: ", 10, 0)

    val rand4K = new Random(seed)
    val parArrayForKEuclid = (seed :: (for (index <- (1 until nbAttemp)) yield rand4K.nextInt(maxSeedParralel)).toList).toParArray
    val kMeanEuclid = exp12FCS.kmeansCompensatedEuclidConv(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, parArrayForKEuclid)
    println("Cluster seed: \t" + parArrayForKEuclid.mkString("\t"))
    println("Cluster quality:\t" + kMeanEuclid.map(x => x._1.last).mkString("\t"))
    show(FCSOutput.kMeanFCSPlotSeqEuclid(kMeanEuclid))
    clusterLoop = scala.io.StdIn.readLine("Retry, Y/[N]?") match {
      case "Y" => true
      case _: String => false
    }
    println("Bye bye")
  }
}

