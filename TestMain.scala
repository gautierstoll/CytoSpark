

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
  println("FCS analyzer, https://github.com/gautierstoll/CytoSpark, version 0.9")
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

  def takeRemoveParam(fcsDataKMean: FCSDataKMean): List[Int] = {
    val askingListParam: String = "Remove Paramters? "+(for (paramAndIndex <- fcsDataKMean.takenParam.zipWithIndex) yield {
      (paramAndIndex._2 + 1).toString + ": " + (try fcsDataKMean.textSegmentMap("$P" + paramAndIndex._1 + "S") catch {
        case _: Throwable => fcsDataKMean.textSegmentMap("$P" + paramAndIndex._1 + "N")
      })
    }).reduce(_ + ", " + _)+ " "
    takeListInt(askingListParam, 1, fcsDataKMean.takenParam.length)
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
        if (retry == "Y") filePromp = true else {
          println("Bye Bye");
          System.exit(0)
        }
      }
      else filePromp = false
    }
    val fcsHeader = new FCSHeader(fcsFile)
    val inputParser = fcsHeader.getOnlineFCSInput
    val parsedFCS = new FCSParserFull(inputParser)
    var clusterLoop = true
    var fcsDataKMean: FCSDataKMean = null
    var nbCluster = 6
    var nbRow: Int = inputParser.takeNbEvent
    var nbIteration: Int = 10
    var nbStep: Int = 5
    while (clusterLoop) {
      if (fcsDataKMean == null) {
        println("Clustering parameters:")
        nbCluster = takeIntFromLine("Number of clusters [6]: ", 6, 1)
        nbRow =
          takeIntFromLine("Number of used rows [" + inputParser.takeNbEvent + "]: ", inputParser.takeNbEvent, 1) match {
            case y: Int => if (y > inputParser.takeNbEvent) {
              println("Take " + inputParser.takeNbEvent)
              inputParser.takeNbEvent
            } else y
          }
        nbIteration =
          takeIntFromLine("Number of K-Mean iterations [10]: ", 10, 1)
        nbStep = takeIntFromLine("Number of K-Mean steps [5]: ", 5, 2)
        val nbAttemp: Int = takeIntFromLine("Number of K-Mean clustering [4]: ", 4, 1)
        val seed: Int =
          takeIntFromLine("Pseudo-random generator initial condition [10]: ", 10, 0)
        val rand4K = new Random(seed)
        val parArrayForKEuclid = (seed :: (for (index <- (1 until nbAttemp)) yield rand4K.nextInt(maxSeedParralel)).toList).
          toParArray
        fcsDataKMean =
          parsedFCS.kmeanFCSEuclidConv(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, parArrayForKEuclid)
        println("Cluster seed: \t" + parArrayForKEuclid.mkString("\t"))
      }
      else
        fcsDataKMean = parsedFCS.kmeanFCSEuclidConvContinue(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, fcsDataKMean.euclidKResult)
      println("Cluster quality:\t" + fcsDataKMean.euclidKResult.map(x => x._1.last).mkString("\t"))
      show(FCSOutput.kMeanFCSPlotSeqEuclid(fcsDataKMean.euclidKResult))
      clusterLoop = scala.io.StdIn.readLine("Retry, (C)ontinue or (P)lot ?") match {
        case "P" => {
          //val bestClusterEuclid = kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2
          var bestClusterList: (List[EllipseClusterId], Array[String]) = null
          var loopPlot = true
          while (loopPlot) {
            val removeCluster =
              this.takeListInt("Remove clusters (separated by ','): ", 1, nbCluster).map(_ - 1).toArray
            val removeParam = this.takeRemoveParam(fcsDataKMean).map(_ - 1).toArray
            scala.io.StdIn.readLine("Scatter, Scatter with (G)rid, (C)luster center, (E)llipse or (T)ree plot? ") match {
              case "C" => {
                val outPdf = scala.io.StdIn.readLine("Cluster file: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotClusters2D(fcsDataKMean, removeCluster,removeParam), outPdf)
              }
              case "E" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataKMean)
                val outPdf = scala.io.StdIn.readLine("Ellipse file: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotEllipse2D(bestClusterList, removeCluster, removeParam), outPdf)
              }
              case "T" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataKMean)
                val ellipseTree = FCSOutput.treeKmeanClust(bestClusterList, removeCluster)
                val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.treeKmeanClustPlot2D(bestClusterList, ellipseTree,removeParam), outPdf)
                if (scala.io.StdIn.readLine("Write tree to file? Y/(N): ") == "Y") {
                  val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
                  FCSOutput.writeClusterTreeSizeCsv(ellipseTree, outCsv)
                }
              }
              case "G" => {
                val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                val gridWidth = takeIntFromLine("Grid size[50]: ", 50, 2)
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlot2DGrid(fcsDataKMean, gridWidth, removeCluster,removeParam), outPdf)
              }
              case _: String => {
                val outPng = scala.io.StdIn.readLine("File: ") + ".png"
                val pngWidth = takeIntFromLine("Png width[2000]: ", 2000, 1000)
                FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(fcsDataKMean, removeCluster,removeParam), outPng, pngWidth)
              }
            }
            if (scala.io.StdIn.readLine("Write cluster sizes to file? Y/(N): ") == "Y") {
              val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
              FCSOutput.writeClusterSizeCsv(fcsDataKMean.bestKMean.clusters, outCsv)
            }
            loopPlot = scala.io.StdIn.readLine("New plot? (Y)/N: ") match {
              case "Y" => true
              case _: String => false
            }
          }
          scala.io.StdIn.readLine("New cluster? (Y)/N: ") match {
            case "Y" => {
              fcsDataKMean = null
              true
            }
            case _: String => false
          }
        }
        case "C" => true
        case _: String => {
          fcsDataKMean = null
          true
        }
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