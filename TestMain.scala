

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
  var loopFile = true
  while (loopFile) {
    val fcsFile = scala.io.StdIn.readLine("FCS File: ")
    if (!Files.exists(Paths.get(fcsFile))) sys.error("File " + fcsFile + " do not exist")
    val fcsHeader = new FCSHeader(fcsFile)
    val inputParser = fcsHeader.getOnlineFCSInput
    val parsedFCS = new FCSParserFull(inputParser)
    var clusterLoop = true
    while (clusterLoop) {
      println("Clustering parameters:")
      val nbCluster: Int = takeIntFromLine("Number of clusters [6]: ", 6, 1)
      val nbRow: Int =
        takeIntFromLine("Number of used rows [" + inputParser.takeNbEvent + "]: ", inputParser.takeNbEvent, 1) match {
          case y: Int => if (y > inputParser.takeNbEvent) {
            println("Take " + inputParser.takeNbEvent)
            inputParser.takeNbEvent
          } else y
        }
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
      //var ellipseTree: List[ClusterEllipse.ArrowEllipseCluster] = null
      clusterLoop = scala.io.StdIn.readLine("Retry or (P)lot ?") match {
        case "P" => {
          val bestClusterEuclid = kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2
          var bestClusterList = null
          var loopPlot = true
          while (loopPlot) {




            loopPlot = scala.io.StdIn.readLine("New plot? (Y)/N; ") match {
              case "Y" => true
              case _: String => false
            }
          }
          scala.io.StdIn.readLine("New cluster? (Y)/N; ") match {
            case "Y" => true
            case _: String => false
          }
        }
        case _: String => true
      }
    }
    loopFile = scala.io.StdIn.readLine("New file? (Y)/N; ") match {
      case "Y" => true
      case _: String => false
    }
  }
  println("Bye bye")
  System.exit(0)
}

while (loopMain.plot) {
  val bestClusterEuclid = kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2

  loopMain.analyzePlot = scala.io.StdIn.readLine("Plot or (A)nalyze plot ?") match {
    case
  }
  case "C"
  => (true, false)
  case "P"
  =>
  {
    scala.io.StdIn.readLine("2D-Scatter, 2D-(C)luster, 2D-(E)llispe, 2D-(T)ree? ") match {
      case "C" => {
        val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
        FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotClusters2D(parsedFCS, bestClusterEuclid), outPdf)
      }
      case "E" => {
        val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
        FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotEllipse2D(parsedFCS, bestClusterEuclid), outPdf)
      }
      case "T" => {
        if (ellipseTree == null) {
          ellipseTree = FCSOutput.treeKmeanClust(parsedFCS, bestClusterEuclid)
        }
        val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
        FCSOutput.plotKSeqToPdf(FCSOutput.treeKmeanClustPlot2D(parsedFCS, ellipseTree), outPdf)
      }
      case _: String => {
        val outPng = scala.io.StdIn.readLine("File: ") + ".png"
        val pngWidth = takeIntFromLine("Png width: ", 1000, 1000)
        FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(parsedFCS, bestClusterEuclid), outPng, pngWidth)
      }
    }
    (true, true)
  }
  case "W"
  =>
  {
    val outCsv = scala.io.StdIn.readLine("File: ") + ".csv"
    if (ellipseTree == null) {
      FCSOutput.writeClusterSizeCsv(bestClusterEuclid.clusters, outCsv)
    } else {
      println("write tree cluster")
      FCSOutput.writeClusterTreeSizeCsv(ellipseTree, outCsv)
    }
    (true, true)
  }
  case _: String => (false, false)
}
}
}
}
println("Bye bye")
System.exit(0)
}

