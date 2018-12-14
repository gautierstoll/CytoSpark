

import java.io._

import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer

import org.nspl.awtrenderer._

import scala.collection.parallel.mutable._
import org.saddle._
import stat._
import java.nio.file.{Files, Paths}
import scala.io.Source

import ClusterEllipse.EllipseClusterId
import LinePromptData._

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

  /**
    *
    * @param fcsDataFinalKMean
    */
  def plottingLoop(fcsDataFinalKMean: FCSDataFinalKMean): Unit = {
    var bestClusterList: (List[EllipseClusterId], Array[String]) = null
    var loopPlot = true
    while (loopPlot) {
      val removeCluster =
        takeListInt("Remove clusters (separated by ','): ", 1, fcsDataFinalKMean.bestKMean.means.length).map(_ - 1).toArray
      val removeParam = takeRemoveParam(fcsDataFinalKMean).map(_ - 1).toArray //removeParam start at 0
      scala.io.StdIn.readLine("- [Scatter], potential large png file\n" +
        "- Scatter with virtual (g)rid, lighter than scatter, pdf file\n" +
        "- (c)luster centers (proportional to size), pdf file \n" +
        "- (e)llipses or potatoes, easy to cook pdf file\n" +
        "- cluster centers (a)nd ellipses pdf file\n " +
        "- (t)ree of clusters, nice organic pdf file\n" +
        "- (n)etwork of clusters, avoiding a poor cluster to be alone, pdf file\n? ") match {
        case "c" => {
          val outPdf = scala.io.StdIn.readLine("Cluster center pdf file: ") + ".pdf"
          FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlotClusters2D(fcsDataFinalKMean, removeCluster, removeParam), outPdf)
        }
        case "e" => {
          if (bestClusterList == null) {
            bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean); println("Compute ellipses\n")
          }
          val outPdf = scala.io.StdIn.readLine("Ellipse file: ") + ".pdf"
          val ellipsePdf = try (FCSOutput.kMeanFCSPlotEllipse2D(bestClusterList, removeCluster, removeParam)) catch {
            case ex: ClusterEllipse.EllipseException => {
              println(ex.errMessage());
              null
            }
          }
          if (ellipsePdf != null)
            FCSOutput.plotKSeqToPdf(ellipsePdf, outPdf)
          if (scala.io.StdIn.readLine("Export ellipses to elcl file? y/[n]: ")=="y") {
            bestClusterList._1.sortWith(_.clusterId < _.clusterId).foreach(cl => cl.promptName())
            val file = scala.io.StdIn.readLine("Export file: ")
            ClusterEllipse.ExportEllipseIdList(file,
              bestClusterList._1.filter(cl => scala.io.StdIn.readLine("Take "+cl.nameId+" ? [y]/n: ") != "n"),
              bestClusterList._2)
          }
        }
        case "a" => {
          if (bestClusterList == null) {
            bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean); println("Compute ellipses\n")
          }
          val outPdf = scala.io.StdIn.readLine("Ellipse and center pdf file: ") + ".pdf"
          val ellipseCenterPdf = try (FCSOutput.kMeanFCSPlotClusterEllipse2D(fcsDataFinalKMean, bestClusterList, removeCluster, removeParam)) catch {
            case ex: ClusterEllipse.EllipseException => {
              println(ex.errMessage());
              null
            }
          }
          if (ellipseCenterPdf != null)
            FCSOutput.plotKSeqToPdf(ellipseCenterPdf, outPdf)
          if (scala.io.StdIn.readLine("Export ellipses to elcl file? y/[n]: ")=="y") {
            bestClusterList._1.sortWith(_.clusterId < _.clusterId).foreach(cl => cl.promptName())
            val file = scala.io.StdIn.readLine("Export file: ")
            ClusterEllipse.ExportEllipseIdList(file,
              bestClusterList._1.filter(cl => scala.io.StdIn.readLine("Take "+cl.nameId+" ? [y]/n: ") != "n"),
              bestClusterList._2)
          }
        }
        case "t" => {
          if (bestClusterList == null) {
            bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean); println("Compute ellipses\n")
          }
          val ellipseTree = try (FCSOutput.treeKmeanClust(bestClusterList, removeCluster, removeParam)) catch {
            case ex: ClusterEllipse.EllipseException => {
              println(ex.errMessage());
              null
            }
          }
          if (ellipseTree != null) {
            val outPdf = scala.io.StdIn.readLine("Tree pdf file: ") + ".pdf"
            FCSOutput.plotKSeqToPdf(
              FCSOutput.networkKmeanClustPlot2D(bestClusterList._2.zipWithIndex.filter(x => !removeParam.contains(x._2)).map(x => x._1), ellipseTree),
              outPdf)

            if (scala.io.StdIn.readLine("Write tree to csv file? y/[n]: ") == "y") {
              val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
              FCSOutput.writeClusterTreeSizeCsv(ellipseTree, outCsv)
            }
          }
        }
        case "n" => {
          if (bestClusterList == null) {
            bestClusterList = FCSOutput.clusterForPlot(fcsDataFinalKMean); println("Compute ellipses\n")
          }
          val ellipseNetwork = try (FCSOutput.connNetworkClust(bestClusterList, removeCluster, removeParam)) catch {
            case ex: ClusterEllipse.EllipseException => {
              println(ex.errMessage());
              null
            }
          }
          if (ellipseNetwork != null) {
            val outPdf = scala.io.StdIn.readLine("Network pdf File: ") + ".pdf"
            FCSOutput.plotKSeqToPdf(
              FCSOutput.networkKmeanClustPlot2D(bestClusterList._2.zipWithIndex.filter(x => !removeParam.contains(x._2)).map(x => x._1), ellipseNetwork),
              outPdf)

            if (scala.io.StdIn.readLine("Write network to csv file? y/[n]: ") == "y") {
              val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
              FCSOutput.writeClusterTreeSizeCsv(ellipseNetwork, outCsv)
            }
          }
        }
        case "g" => {
          val outPdf = scala.io.StdIn.readLine("Grid pdf file: ") + ".pdf"
          val gridWidth = takeIntFromLine("Grid size[50]: ", 50, 2)
          FCSOutput.plotKSeqToPdf(FCSOutput.kMeanFCSPlot2DGrid(fcsDataFinalKMean, gridWidth, removeCluster, removeParam), outPdf)
        }
        case _: String => {
          val outPng = scala.io.StdIn.readLine("Scatter png file: ") + ".png"
          val pngWidth = takeIntFromLine("Png width[2000]: ", 2000, 1000)
          FCSOutput.plotKSeqToPng(FCSOutput.kMeanFCSPlot2D(fcsDataFinalKMean, removeCluster, removeParam), outPng, pngWidth)
        }
      }
      if (scala.io.StdIn.readLine("Write cluster sizes to csv file? y/[n]: ") == "y") {
        val outCsv = scala.io.StdIn.readLine("Csv file: ") + ".csv"
        if (bestClusterList == null) FCSOutput.writeClusterSizeCsv(fcsDataFinalKMean.bestKMean.clusters, outCsv) else {
          FCSOutput.writeClusterSizeCsv(fcsDataFinalKMean.bestKMean.clusters, outCsv,bestClusterList._1.sortWith(_.clusterId < _.clusterId).map(_.nameId).toArray)
        }
      }
      loopPlot = scala.io.StdIn.readLine("New plot? [y]/n: ") match {
        case "n" => false
        case _: String => true
      }
    }
  }

  def kMeanFCSClustering(parsedFCS : FCSParserFull, takeRows : Array[Int]) : FCSDataFinalKMean = {
    println("Clustering parameters:")
    val nbCluster =
      takeIntFromLine("Number of clusters [6] (large number takes more time to compute, but associated potatoes are easier to cook): ", 6, 1)
    val nbIteration =
      takeIntFromLine("Number of K-Mean iterations [10]: ", 10, 1)
    val nbStep = takeIntFromLine("Number of K-Mean steps (ie blocks of K-mean iterations) [5]: ", 5, 2)
    val nbAttemp: Int = takeIntFromLine("Number of K-Mean clustering (done in parallel) [4]: ", 4, 1)
    val seed: Int =
      takeIntFromLine("Pseudo-random generator initial condition [10]: ", 10, 0)
    val rand4K = new Random(seed)
    val parArrayForKEuclid = (seed :: (for (index <- (1 until nbAttemp)) yield rand4K.nextInt(maxSeedParralel)).toList).
      toParArray
    var fcsDataParKMean =
      parsedFCS.kmeanPPFCSEuclidConv(KMeanFCSInput(nbCluster, takeRows, nbIteration, 0), nbStep, parArrayForKEuclid)
    println("Cluster seed: \t" + parArrayForKEuclid.mkString("\t"))
    println("Cluster quality:\t" + fcsDataParKMean.euclidKResult.map(x => x._1.last).mkString("\t"))
    show(FCSOutput.kMeanFCSPlotSeqEuclid(fcsDataParKMean.euclidKResult))
    while (scala.io.StdIn.readLine("Continue clustering? y/[n]") == "y") {
          fcsDataParKMean = parsedFCS.kmeanFCSEuclidConvContinue(KMeanFCSInput(nbCluster, takeRows, nbIteration, 0), nbStep, fcsDataParKMean.euclidKResult)
          println("Cluster quality:\t" + fcsDataParKMean.euclidKResult.map(x => x._1.last).mkString("\t"))
          show(FCSOutput.kMeanFCSPlotSeqEuclid(fcsDataParKMean.euclidKResult))
    }
    FCSDataFinalKMean(fcsDataParKMean)
  }

  val maxSeedParralel = 10000 //limit to make it handlable by the user

  println("FCS analyzer, by Gautier Stoll, version 0.9")
  println("Code: https://github.com/gautierstoll/CytoSpark, version 0.9. Because I am not a professional, I think my code is quite ugly...")
  println("Written in Scala because it is fun. Hope it is also fast.")

  if (scala.io.StdIn.readLine("Generate cluster or use them? [g]/u: ") == "u") {

  }
  else {
    var loopFile = true
    var fcsFile: String = ""
    while (loopFile) {
      fcsFile = askFileFromType("fcs")
      val fcsHeader = new FCSHeader(fcsFile)
      val inputParser = fcsHeader.getOnlineFCSInput
      val parsedFCS = new FCSParserFull(inputParser)

      var clusterLoop = true
      while (clusterLoop) {
        val nbRow =
          takeIntFromLine("Number of used rows [" + inputParser.takeNbEvent + "]: ", inputParser.takeNbEvent, 1,inputParser.takeNbEvent)
        var fcsDataFinalKMean = kMeanFCSClustering(parsedFCS, (0 until nbRow).toArray) //var because possible subclustering
        println("Now, let's see how these clusters look like...")
        plottingLoop(fcsDataFinalKMean)
        while (scala.io.StdIn.readLine("Sub-clustering? y/[n]: ") == "y") {
          val subClusterIndex = takeIntFromLine("Cluster to separate: ", 1, 0, fcsDataFinalKMean.bestKMean.means.length) - 1
          val subClusterDataIndices = fcsDataFinalKMean.bestKMean.clusters.toSeq.zipWithIndex.filter(x => x._1 == subClusterIndex).map(_._2).toArray
          val subCluster = kMeanFCSClustering(parsedFCS, subClusterDataIndices).bestKMean
          val newFCSDataFinalKMean = fcsDataFinalKMean.subClustering(subClusterIndex, subCluster)
          println("Now, let's see if it looks better")
          plottingLoop(newFCSDataFinalKMean)
          if (scala.io.StdIn.readLine("Happy with this sub-clustering, apply it? [y]/n: ") != "n") fcsDataFinalKMean = newFCSDataFinalKMean
        }
        clusterLoop = scala.io.StdIn.readLine("New cluster? y/[n]: ") match {
          case "y" => true
          case _: String => false
        }
      }
      loopFile = scala.io.StdIn.readLine("New file? y/[n]: ") match {
        case "y" => true
        case _: String => false
      }
    }
  }
  println("Bye bye")
  System.exit(0)
}