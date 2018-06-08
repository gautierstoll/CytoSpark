

import java.io._

import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer

import org.nspl.awtrenderer._

import scala.collection.parallel.mutable._
import org.saddle._
import stat._
import java.nio.file.{Files, Paths}

import ClusterEllipse.EllipseClusterId

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
import scala.tools.nsc.transform.patmat.Lit

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

  def takeListInt(askingPromp: String, minVal: Int, maxVal: Int): List[Int] = {
    scala.io.StdIn.readLine(askingPromp).
      toCharArray.filter(_ != ' ').mkString("").split(",").
      map(x => try (x.toInt) catch {
        case _: Throwable => (minVal - 1)
      }).toList.filter(x => (x <= maxVal) && (x >= minVal))
  }

  var loopFile = true
  var fcsFile: String = ""
  while (loopFile) {
    var filePromp = true
    while (filePromp) {
      fcsFile = scala.io.StdIn.readLine("FCS File: ")
      if (!Files.exists(Paths.get(fcsFile))) {
        println("File " + fcsFile + " do not exist")
        val retry = scala.io.StdIn.readLine("Retry? (Y)/N ")
        if (retry == "Y") filePromp = true else System.exit(0)
      }
      else filePromp = false
    }
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
      clusterLoop = scala.io.StdIn.readLine("Retry or (P)lot ?") match {
        case "P" => {
          val bestClusterEuclid = kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2
          var bestClusterList: (List[EllipseClusterId], Array[String]) = null
          var loopPlot = true
          while (loopPlot) {
            val removeCluster =
              this.takeListInt("Remove clusters (separated by ','): ", 1, nbCluster).map(_ - 1).toArray
            scala.io.StdIn.readLine("Scatter, (C)luster, (E)llipse or (T)ree plot? ") match {
              case "C" => {
                val outPdf = scala.io.StdIn.readLine("Cluster file: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotClusters2D(parsedFCS, bestClusterEuclid, removeCluster), outPdf)
              }
              case "E" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(parsedFCS, bestClusterEuclid)
                val outPdf = scala.io.StdIn.readLine("Ellipse file: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotEllipse2D(bestClusterList, removeCluster), outPdf)
              }
              case "T" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(parsedFCS, bestClusterEuclid)
                val ellipseTree = FCSOutput.treeKmeanClust(bestClusterList, removeCluster)
                val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.treeKmeanClustPlot2D(bestClusterList, ellipseTree), outPdf)
                if (scala.io.StdIn.readLine("Write tree to file? Y/(N): ") == "Y") {
                  val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
                  FCSOutput.writeClusterTreeSizeCsv(ellipseTree, outCsv)
                }
              }
              case _: String => {
                val outPng = scala.io.StdIn.readLine("File: ") + ".png"
                val pngWidth = takeIntFromLine("Png width: ", 1000, 1000)
                FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(parsedFCS, bestClusterEuclid, removeCluster), outPng, pngWidth)
              }
            }
            if (scala.io.StdIn.readLine("Write cluster sizes to file? Y/(N): ") == "Y") {
              val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
              FCSOutput.writeClusterSizeCsv(bestClusterEuclid.clusters, outCsv)
            }
            loopPlot = scala.io.StdIn.readLine("New plot? (Y)/N: ") match {
              case "Y" => true
              case _: String => false
            }
          }
          scala.io.StdIn.readLine("New cluster? (Y)/N: ") match {
            case "Y" => true
            case _: String => false
          }
        }
        case _: String => true
      }
    }
    loopFile = scala.io.StdIn.readLine("New file? (Y)/N: ") match {
      case "Y" => true
      case _: String => false
    }
  }

  println("Bye bye")
  System.exit(0)
}