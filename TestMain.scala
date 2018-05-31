

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
        println("Take min " + minVal)
        minVal
      }
      else y
    }
  }

  //def main() {
  val fcsFile = scala.io.StdIn.readLine("FCS File: ")
  if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " do not exist")

  val fcsHeader = new FCSHeader(fcsFile)
  val inputParser = fcsHeader.getOnlineFCSInput

  val parsedFCS = new FCSParserFull(inputParser)
  var clusterExportLoop: (Boolean, Boolean) = (true, true)
  while (clusterExportLoop._1) {
    println("Clustering parameters:")
    val nbCluster: Int = takeIntFromLine("Number of clusters [6]: ", 6, 1)
    val nbRow: Int =
      takeIntFromLine("Number of used rows [" + parsedFCS.nbEvent + "]: ", parsedFCS.nbEvent, 1)
    val nbIteration: Int =
      takeIntFromLine("Number of K-Mean iterations [100]: ", 100, 1)
    val nbStep: Int = takeIntFromLine("Number of K-Mean steps [5]: ", 5, 2)
    val nbAttemp: Int = takeIntFromLine("Number of K-Mean clustering [5]: ", 5, 1)
    val seed: Int =
      takeIntFromLine("Pseudo-random generator initial condition [10]: ", 10, 0)

    val rand4K = new Random(seed)
    val parArrayForKEuclid = (seed :: (for (index <- (1 until nbAttemp)) yield rand4K.nextInt(maxSeedParralel)).toList).
      toParArray
    val kMeanEuclid =
      parsedFCS.kmeansFCSEuclidConv(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, parArrayForKEuclid)
    println("Cluster seed: \t" + parArrayForKEuclid.mkString("\t"))
    println("Cluster quality:\t" + kMeanEuclid.map(x => x._1.last).mkString("\t"))
    show(FCSOutput.kMeanFCSPlotSeqEuclid(kMeanEuclid))
    clusterExportLoop = (true, true)
    while (clusterExportLoop._2) {
      clusterExportLoop = scala.io.StdIn.readLine("(C)luster, (P)lot, (W)rite to file, Quit?") match {
        case "C" => (true, false)
        case "P" => {
          scala.io.StdIn.readLine("2D-(S)catter or 2D-Cluster? ") match {
            case "S" => {
              val outPng = scala.io.StdIn.readLine("File: ") + ".png"
              val pngWidth = takeIntFromLine("Png width: ",1000,1000)
              FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(parsedFCS,
                kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2), outPng,pngWidth)
            }
            case _: String => {
              val outPng = scala.io.StdIn.readLine("File: ") + ".png"
              val pngWidth = takeIntFromLine("Png width: ",1000,1000)
              FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlotClusters2D(parsedFCS,
                kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2), outPng, pngWidth)
            }
          }
          (true, true)
        }
        case "W" => {
          val outCsv = scala.io.StdIn.readLine("File: ") + ".csv"
          FCSOutput.writeClusterSizeCsv(kMeanEuclid.
            filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2.clusters,outCsv)
          (true, true)
        }
        case _: String => (false, false)
      }
    }
  }
  println("Bye bye")
  System.exit(0)
}

