

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
  println("FCS analyzer, by Gautier Stoll, version 0.9")
  println("Code: https://github.com/gautierstoll/CytoSpark, version 0.9. Because I am not a professional, I think my code is quite ugly...")
  println("Written in Scala because it is fun. Hope it is also fast.")

  val maxSeedParralel = 10000 //limit to make it handlable by the user

  def takeIntFromLine(askingPromp: String, defaultVal: Int, minVal: Int): Int = {
    val takenInt = scala.io.StdIn.readLine(askingPromp) match {
      case "" => defaultVal
      case x: String => try {
        x.toInt
      } catch {
        case _: Throwable => println("Take default " + defaultVal); defaultVal
      }
    }
    if (takenInt < minVal) {
      println("Take min " + minVal); minVal
    } else takenInt
  }

  def takeListInt(askingPromp: String, minVal: Int, maxVal: Int): List[Int] = { //seperated by comma, remove unreadable Int
    scala.io.StdIn.readLine(askingPromp).
      toCharArray.filter(_ != ' ').mkString("").split(",").
      map(x => try (x.toInt) catch {
        case _: Throwable => (minVal - 1)
      }).toList.filter(x => (x <= maxVal) && (x >= minVal))
  }

  def takeRemoveParam(fcsDataParKMean: FCSDataParKMean): List[Int] = {
    val askingListParam: String = "Remove Parameters? " + (for (paramAndIndex <- fcsDataParKMean.takenParam.zipWithIndex) yield {
      (paramAndIndex._2 + 1).toString + ": " + (try fcsDataParKMean.textSegmentMap("$P" + paramAndIndex._1 + "S") catch {
        case _: Throwable => fcsDataParKMean.textSegmentMap("$P" + paramAndIndex._1 + "N")
      })
    }).reduce(_ + ", " + _) + " "
    takeListInt(askingListParam, 1, fcsDataParKMean.takenParam.length)
  }

  def askFileOrNull():String = {
      val fcsFile = scala.io.StdIn.readLine("FCS File: ")
      if (!Files.exists(Paths.get(fcsFile))) {
        println("File " + fcsFile + " do not exist")
        val retry = scala.io.StdIn.readLine("Retry? y/[n] ")
        if (retry == "Y") askFileOrNull() else null
      }
      else fcsFile
  }


  var loopFile = true
  var fcsFile: String = ""
  while (loopFile) {
//    var filePromp = true
//    while (filePromp) {
//      fcsFile = scala.io.StdIn.readLine("FCS File: ")
//      if (!Files.exists(Paths.get(fcsFile))) {
//        println("File " + fcsFile + " do not exist")
//        val retry = scala.io.StdIn.readLine("Retry? y/[n] ")
//        if (retry == "Y") filePromp = true else {
//          println("Bye Bye");
//          System.exit(0)
//        }
//      }
//      else filePromp = false
//    }
    fcsFile = askFileOrNull()
    if (fcsFile == null) {println("Bye Bye \n");System.exit(0)}

    val fcsHeader = new FCSHeader(fcsFile)
    val inputParser = fcsHeader.getOnlineFCSInput
    val parsedFCS = new FCSParserFull(inputParser)
    var clusterLoop = true
    var fcsDataParKMean: FCSDataParKMean = null
    var nbCluster = 6
    var nbRow: Int = inputParser.takeNbEvent
    var nbIteration: Int = 10
    var nbStep: Int = 5
    while (clusterLoop) {
      if (fcsDataParKMean == null) {
        println("Clustering parameters:")
        nbCluster = takeIntFromLine("Number of clusters [6] (large number takes more time to compute, but associated potatoes are easier to cook): ", 6, 1)
        nbRow =
          takeIntFromLine("Number of used rows [" + inputParser.takeNbEvent + "]: ", inputParser.takeNbEvent, 1) match {
            case y: Int => if (y > inputParser.takeNbEvent) {
              println("Take " + inputParser.takeNbEvent)
              inputParser.takeNbEvent
            } else y
          }
        nbIteration =
          takeIntFromLine("Number of K-Mean iterations [10]: ", 10, 1)
        nbStep = takeIntFromLine("Number of K-Mean steps (ie blocks of K-mean iterations) [5]: ", 5, 2)
        val nbAttemp: Int = takeIntFromLine("Number of K-Mean clustering (done in parallel) [4]: ", 4, 1)
        val seed: Int =
          takeIntFromLine("Pseudo-random generator initial condition [10]: ", 10, 0)
        val rand4K = new Random(seed)
        val parArrayForKEuclid = (seed :: (for (index <- (1 until nbAttemp)) yield rand4K.nextInt(maxSeedParralel)).toList).
          toParArray
        fcsDataParKMean =
          parsedFCS.kmeanPPFCSEuclidConv(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, parArrayForKEuclid)
        println("Cluster seed: \t" + parArrayForKEuclid.mkString("\t"))
      }
      else
        fcsDataParKMean = parsedFCS.kmeanFCSEuclidConvContinue(KMeanFCSInput(nbCluster, nbRow, nbIteration, 0), nbStep, fcsDataParKMean.euclidKResult)
      println("Cluster quality:\t" + fcsDataParKMean.euclidKResult.map(x => x._1.last).mkString("\t"))
      show(FCSOutput.kMeanFCSPlotSeqEuclid(fcsDataParKMean.euclidKResult))
      clusterLoop = scala.io.StdIn.readLine("[Retry], (c)ontinue or (p)lot ?") match {
        case "p" => {
          val fcsDataFinalKMean: FCSDataFinalKMean = new FCSDataFinalKMean(fcsDataParKMean)
          //val bestClusterEuclid = kMeanEuclid.filter(x => x._1.last == kMeanEuclid.map(x => x._1.last).min).head._2
          var bestClusterList: (List[EllipseClusterId], Array[String]) = null
          var loopPlot = true
          while (loopPlot) {
            val removeCluster =
              this.takeListInt("Remove clusters (separated by ','): ", 1, nbCluster).map(_ - 1).toArray
            val removeParam = this.takeRemoveParam(fcsDataParKMean).map(_ - 1).toArray //removeParam start at 0
            scala.io.StdIn.readLine("- [Scatter], potential large png file\n" +
              "- Scatter with virtual (g)rid, lighter than scatter, pdf file\n" +
              "- (c)luster center, pdf file \n" +
              "- (e)llipse or potatoes, easy to cook pdf file\n" +
              "- cluster center (a)nd ellipse pdf file\n " +
              "- (t)ree of cluster, nice organic pdf file\n" +
              "- (n)etwork of cluster, avoiding a poor cluster to be alone, pdf file\n? ") match {
              case "c" => {
                val outPdf = scala.io.StdIn.readLine("Cluster file: ") + ".pdf"
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotClusters2D(fcsDataFinalKMean, removeCluster, removeParam), outPdf)
              }
              case "e" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean)
                val outPdf = scala.io.StdIn.readLine("Ellipse file: ") + ".pdf"
                val ellipsePdf = try (FCSOutput.kMeanFCSPlotEllipse2D(bestClusterList, removeCluster, removeParam)) catch {
                  case ex: ClusterEllipse.EllipseException => {
                    println(ex.errMessage());
                    null
                  }
                }
                if (ellipsePdf != null)
                  FCSOutput.plotKSeqToPdf(ellipsePdf, outPdf)
              }
              case "a" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean)
                val outPdf = scala.io.StdIn.readLine("Ellipse and center file: ") + ".pdf"
                val ellipseCenterPdf = try (FCSOutput.kMeanFCSPlotClusterEllipse2D(fcsDataFinalKMean,bestClusterList, removeCluster, removeParam)) catch {
                  case ex: ClusterEllipse.EllipseException => {
                    println(ex.errMessage());
                    null
                  }
                }
                if (ellipseCenterPdf != null)
                  FCSOutput.plotKSeqToPdf(ellipseCenterPdf, outPdf)
              }
              case "t" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean)
                val ellipseTree = try (FCSOutput.treeKmeanClust(bestClusterList, removeCluster, removeParam)) catch {
                  case ex: ClusterEllipse.EllipseException => {
                    println(ex.errMessage());
                    null
                  }
                }
                if (ellipseTree != null) {
                  val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                  FCSOutput.plotKSeqToPdf(
                    FCSOutput.networkKmeanClustPlot2D(bestClusterList._2.zipWithIndex.filter(x => !removeParam.contains(x._2)).map(x => x._1), ellipseTree),
                    outPdf)

                  if (scala.io.StdIn.readLine("Write tree to file? y/[n]: ") == "y") {
                    val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
                    FCSOutput.writeClusterTreeSizeCsv(ellipseTree, outCsv)
                  }
                }
              }
              case "n" => {
                if (bestClusterList == null) bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean)
                val ellipseNetwork = try (FCSOutput.connNetworkClust(bestClusterList, removeCluster, removeParam)) catch {
                  case ex: ClusterEllipse.EllipseException => {
                    println(ex.errMessage());
                    null
                  }
                }
                if (ellipseNetwork != null) {
                  val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                  FCSOutput.plotKSeqToPdf(
                    FCSOutput.networkKmeanClustPlot2D(bestClusterList._2.zipWithIndex.filter(x => !removeParam.contains(x._2)).map(x => x._1), ellipseNetwork),
                    outPdf)

                  if (scala.io.StdIn.readLine("Write network to file? y/[n]: ") == "y") {
                    val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
                    FCSOutput.writeClusterTreeSizeCsv(ellipseNetwork, outCsv)
                  }
                }
              }
              case "g" => {
                val outPdf = scala.io.StdIn.readLine("File: ") + ".pdf"
                val gridWidth = takeIntFromLine("Grid size[50]: ", 50, 2)
                FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlot2DGrid(fcsDataFinalKMean, gridWidth, removeCluster, removeParam), outPdf)
              }
              case _: String => {
                val outPng = scala.io.StdIn.readLine("File: ") + ".png"
                val pngWidth = takeIntFromLine("Png width[2000]: ", 2000, 1000)
                FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(fcsDataFinalKMean, removeCluster, removeParam), outPng, pngWidth)
              }
            }
            if (scala.io.StdIn.readLine("Write cluster sizes to file? y/[n]: ") == "y") {
              val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
              FCSOutput.writeClusterSizeCsv(fcsDataFinalKMean.bestKMean.clusters, outCsv)
            }
            loopPlot = scala.io.StdIn.readLine("New plot? [y]/n: ") match {
              case "n" => false
              case _: String => true
            }
          }
          scala.io.StdIn.readLine("New cluster? [y]/n: ") match {
            case "n" => false
            case _: String => {
              fcsDataParKMean = null
              true
            }
          }
        }
        case "c" => true
        case _: String => {
          fcsDataParKMean = null
          true
        }
      }
    }
    loopFile = scala.io.StdIn.readLine("New file? y/[n]: ") match {
      case "y" => true
      case _: String => false
    }
  }
  println("Bye bye")
  System.exit(0)
}