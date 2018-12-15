
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
import shapeless.Inr
import stat.kmeans._
import stat.sparse.SMat

import scala.collection.parallel.mutable._
import LinePromptData._

import scala.annotation.meta.param
import scala.io.Source

object ClusterEllipse {

  /** Exception for cluster of size 1 and/or zero variance
    *
    * @param listExceptionClId
    */
  class EllipseException(listExceptionClId: List[EllipseClusterId]) extends Exception() {
    val sizeId : List[(Int,Int)]= listExceptionClId.filter(clId => clId.cluster.size == 1).map(clId => (clId.cluster.size, clId.clusterId))
    val zeroVarId = listExceptionClId.filter(clId => ((clId.cluster.ellipseMat == null) && (clId.cluster.size > 1))).
      map(clId => (clId.cluster.zeroVarIndex, clId.clusterId))

    def errMessage(): String = {
      "Ellipse cluster exception: \n" +
        sizeId.map(x => "Size: " + x._1.toString + " of cluster " + (x._2 + 1).toString).mkString("\n") +
        " Zero variance of " +
        zeroVarId.map(x => "Indices " + (x._1).map(_ + 1).mkString(",") + " of cluster " + (x._2 + 1).toString).mkString("\n")
    }
  }

  /** write ellipse clusters to a file
    *
    * @param ellipseFile
    * @param listEllipse
    * @param paramNames
    */
  def ExportEllipseIdList(ellipseFile : String,listEllipse : List[EllipseClusterId],paramNames: Array[String]) = {
    val file = new File(ellipseFile+".elcl")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(listEllipse.map(_.toHexString(paramNames)).mkString("\n"))
    bw.close()
  }

  /** cluster defined by mean and variance. Ellipse matrix (inverse of variance) is computed when not provided
    *
    * @param size
    * @param mean
    * @param varMat
    * @param giveEllispeMat
    */
  case class EllipseCluster(size: Int, mean: Array[Double], varMat: DenseMatrix[Double], giveEllispeMat: DenseMatrix[Double] = null) {
    val ellipseMat :DenseMatrix[Double] = if (giveEllispeMat == null) {
      try inv(varMat) catch {
        case _: Throwable => {
          println("Impossible to invert matrix, may cause problems for ellipse/tree")
          println("Data size: " + size)
          println(varMat)
          null
        }
      }
    } else giveEllispeMat
    val zeroVarIndex = (0 until varMat.cols).filter(x => varMat(x, x) == 0)
  }

  /** Ellipse cluster with integer Id, with (mutable) name.
    *
    * @param cluster
    * @param clusterId
    */
  case class EllipseClusterId(cluster: EllipseCluster, clusterId: Int) {
    def double2Hex(db : Double) : String = java.lang.Double.toHexString(db)
    var nameId : String = (clusterId+1).toString // ugly, will disapear
    def toHexString(paramNames : Array[String]): String = {
      "Parameters=" + paramNames.mkString(":") + ";Name=" + nameId + ";Size=" + cluster.size.toString +
        "\nMeans=" + cluster.mean.map(x => double2Hex(x)).mkString(":") +
        "\nVar=" + cluster.varMat.toArray.map(x => double2Hex(x)).mkString(":") +
        "\nEllispe=" + cluster.varMat.toArray.map(x => double2Hex(x)).mkString(":") +
        "\n\\**\\"
    }

    def promptName() = {
      nameId = {
        val nm = scala.io.StdIn.readLine("Name for cluster " + nameId + " :")
        if (nm == "") nameId else nm
      }
    }
  }

  case class EllipseClustering(listEllipse: List[EllipseClusterId], param: Array[String], names: List[String]) {
    def this(tElCl : (List[EllipseClusterId],Array[String],List[String])) = this(tElCl._1,tElCl._2,tElCl._3)

    def this(listFileNames: List[String]) = this(EllipseClustering.hexFilesToEllipses(listFileNames))
    //def this(fcsDataFinalKMean: FCSDataFinalKMean) = this(EllipseClustering.finalKMeanToEllipse(fcsDataFinalKMean))
  }
  object EllipseClustering {
     private def hexFilesToEllipses(listFileNames: List[String]): (List[EllipseClusterId], Array[String], List[String]) = {
      val elclFileList = askListFileFromType("elcl")
      val readEllispeClustersParam: List[(EllipseCluster, Array[String],String)] = elclFileList.flatMap(file => Source.fromFile(file).
        getLines.toList.zipWithIndex.groupBy(_._2 / 5).toSeq.sortWith(_._1 < _._1).map(_._2.map(_._1))).
        map(fiveLines => ClusterEllipse.hexStringToElClusterIdParam(fiveLines))
      val commonParam = readEllispeClustersParam.map(_._2.toSet).reduce(_.intersect(_))
      if (commonParam.size < readEllispeClustersParam.map(_._2.length).max) {
        println("incompatible elcl files"); (null, null, null) // should be less strict?
      }
      else {
        // reorder ellipses,should test it
        val listParam: Array[String] = readEllispeClustersParam.head._2
        val listNameId = readEllispeClustersParam.map(_._3)
        val listEllipseCluster = readEllispeClustersParam.map(elParamName => {
          val reordIndex = listParam.map(elParamName._2.indexOf(_)).toSeq
          EllipseCluster(elParamName._1.size,
            reordIndex.map(elParamName._1.mean(_)).toArray,
            elParamName._1.varMat(reordIndex,reordIndex).toDenseMatrix,
          elParamName._1.giveEllispeMat(reordIndex,reordIndex).toDenseMatrix)
        })
        (listEllipseCluster.zipWithIndex.map(x => EllipseClusterId(x._1,x._2)),listParam,listNameId)
      }

    }
   // private def finalKMeanToEllipse(fcsDataFinalKMean : FCSDataFinalKMean) : (List[EllipseClusterId], Array[String], List[String])


  }

  /** Reconstruction of ellipse cluster for HexString
    *
    * @param hxString
    * @return cluster, paramters list and name
    */
  def hexStringToElClusterIdParam(hxString : List[String]) : (EllipseCluster,Array[String],String) = {
    val patternParam = """Parameters=([^;]*);""".r
    val paramCatch = patternParam.findAllIn(hxString(0)).matchData.toArray
    val parameters : Array[String] = if (paramCatch.length > 0) {
      paramCatch.head.group(1).split(":")
    } else throw new MatchError("Do not find Parameters")

    val patternName = """Name=([^;]*);""".r
    val nameCatch = patternName.findAllIn(hxString(0)).matchData.toArray
    val nameId : String = if (nameCatch.length > 0) {
      nameCatch.head.group(1)
    } else throw new MatchError("Do not find Name")


    val sizeName = """Size=([^;]*);""".r
    val sizeCatch = sizeName.findAllIn(hxString(0)).matchData.toArray
    val clusterSize : Int = if (sizeCatch.length > 0) {
      try sizeCatch.head.group(1).toInt catch {
        case _: Throwable => throw new MatchError("Cannot produce size int")
      }
    }
    else throw new MatchError("Do not find Size")

    val meansName = """Means=([^;]*);""".r
    val meansCatch = meansName.findAllIn(hxString(1)).matchData.toArray
    val clusterMeans : Array[Double] = if (meansCatch.length > 0) {
      try meansCatch.head.group(1).split(":").map(s => java.lang.Double.valueOf(s).toDouble) catch {
        case _: Throwable => throw new MatchError("Cannot produce Means double")
      }
    }
    else throw new MatchError("Do not find Means")

    val varName = """Var=([^;]*);""".r
    val varCatch = varName.findAllIn(hxString(2)).matchData.toArray
    val clusterVar : Array[Double] = if (varCatch.length > 0) {
      try varCatch.head.group(1).split(":").map(s => java.lang.Double.valueOf(s).toDouble) catch {
        case _: Throwable => throw new MatchError("Cannot produce Means double")
      }
    }
    else throw new MatchError("Do not find Means")
    val clusterVarMat = try new DenseMatrix[Double](parameters.length, parameters.length, clusterVar) catch {
      case _: Throwable => throw new MatchError("Wrong dimension of var matrix")
    }

    val ellipseName = """Ellipse=([^;]*);""".r
    val ellipseCatch = ellipseName.findAllIn(hxString(3)).matchData.toArray
    val clusterEllipse : Array[Double] = if (ellipseCatch.length > 0) {
      try ellipseCatch.head.group(1).split(":").map(s => java.lang.Double.valueOf(s).toDouble) catch {
        case _: Throwable => throw new MatchError("Cannot produce Means double")
      }
    }
    else throw new MatchError("Do not find Means")
    val clusterEllipseMat = try new DenseMatrix[Double](parameters.length, parameters.length, clusterEllipse) catch {
      case _: Throwable => throw new MatchError("Wrong dimension of var matrix")
    }
    (EllipseCluster(clusterSize,clusterMeans,clusterVarMat,clusterEllipseMat),parameters,nameId)
  }

  /** fusion of 2 populations defined by size, mean and variance
    *
    * @param clusterA
    * @param clusterB
    * @return
    */
  def fusionEllipseCluster(clusterA: EllipseCluster, clusterB: EllipseCluster): EllipseCluster = {
    val sizeFus = clusterA.size + clusterB.size
    val meanFus = (clusterA.mean).zip(clusterB.mean).
      map(x => (x._1 * clusterA.size + x._2 * clusterB.size) / sizeFus)
    val sumX2A = (clusterA.varMat.map(x => x * (clusterA.size) / (clusterA.size - 1)) +
      (DenseMatrix(clusterA.mean).t) * DenseMatrix(clusterA.mean)).map(x => x * (clusterA.size))
    val sumX2B = (clusterB.varMat.map(x => x * (clusterB.size) / (clusterB.size - 1)) +
      (DenseMatrix(clusterB.mean).t) * DenseMatrix(clusterB.mean)).map(x => x * (clusterB.size))
    val varFus = ((sumX2A + sumX2B).
      map(x => x / (sizeFus)) - (DenseMatrix(meanFus).t) * DenseMatrix(meanFus)).map(x => x * sizeFus / (sizeFus - 1))
    EllipseCluster(sizeFus, meanFus, varFus)
  }

  /** Distance of a point to the center of an ellipse, defined by the ellipse matrix
    *
    * @param point
    * @param ellipseCluster
    * @return
    */
  def distEllipseCluster(point : Array[Double],ellipseCluster : EllipseCluster) : Double = {
    (DenseMatrix(point)*(ellipseCluster.ellipseMat)*(DenseMatrix(point).t)).apply(0,0)}

  /** Distance between ellipses
    * take a point, compute sum of distance to bothe ellipses, take min. Exact formula.
    * @param clusterA
    * @param clusterB
    * @return
    */
  def distEllipseCluster(clusterA: EllipseCluster, clusterB: EllipseCluster): Double = {
    val minVect = inv(clusterA.ellipseMat + clusterB.ellipseMat) *
      (clusterA.ellipseMat * (DenseMatrix(clusterA.mean).t) + clusterB.ellipseMat * (DenseMatrix(clusterB.mean).t))

    (((minVect - DenseMatrix(clusterA.mean).t).t) * clusterA.ellipseMat * (minVect - DenseMatrix(clusterA.mean).t) +
      ((minVect - DenseMatrix(clusterB.mean).t).t) * clusterB.ellipseMat * (minVect - DenseMatrix(clusterB.mean).t)).apply(0, 0)
  }

  /** Arrow from an ellipse to another
    *
    * @param source
    * @param target
    */
  case class ArrowEllipseCluster(source: EllipseClusterId, target: EllipseClusterId) {}

  /**
    * the treeEllipseCluster create a new fusion cluster with a maxId above the max of Ids. This could overload the Ids
    * if the method is applied on a clusterCutList on which some cluster has been removed by another method
    * @param clusterCutList
    * @return
    */
  def treeEllipseCluster(clusterCutList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val maxId = clusterCutList.map(x => x.clusterId).max
    val clusterCutListNoOne = clusterCutList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterCutListNoOne.length == 1) Nil
    else {
      val minDistList = (for (g <- clusterCutListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterCutListNoOne(g(0)).cluster, clusterCutListNoOne(g(1)).cluster)
          , clusterCutListNoOne(g(0)).clusterId, clusterCutListNoOne(g(1)).clusterId)
      }).toList
      val minDist = minDistList.map(x => x._1).min
      val removeClusterId = minDistList.filter(x => (x._1 == minDist)).map(x => (x._2, x._3)).head
      val clusterA = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._1)).head
      val clusterB = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._2)).head
      val newCluster = EllipseClusterId(fusionEllipseCluster(clusterA.cluster, clusterB.cluster), maxId + 1)
      println("Construct fusion cluster " + (maxId + 1 + 1) + " from " + (removeClusterId._1 + 1) +
        " and " + (removeClusterId._2 + 1) + ", distance:" + minDist)
      val newClusterList = newCluster ::
        (clusterCutListNoOne.filter(x => ((x.clusterId != removeClusterId._1) && (x.clusterId != removeClusterId._2))))
      ArrowEllipseCluster(clusterA, newCluster) :: ArrowEllipseCluster(clusterB, newCluster) ::
        treeEllipseCluster(newClusterList)
    }
  }

  /**
    * construct tree level by level, from initial cluster as leaves, not tested yet
    * @param clusterCutList
    * @return
    */
  def levelTreeEllipseCluster(clusterCutList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val maxId = clusterCutList.map(x => x.clusterId).max
    val clusterCutListNoOne = clusterCutList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterCutListNoOne.length == 1) Nil
    else {
      val minDistList: List[(Double, Int, Int)] = (for (g <- clusterCutListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterCutListNoOne(g(0)).cluster, clusterCutListNoOne(g(1)).cluster)
          , clusterCutListNoOne(g(0)).clusterId, clusterCutListNoOne(g(1)).clusterId)
      }).toList

      //closure for one level step
      def levelStep(distClustId: List[(Double, Int, Int)]): List[ArrowEllipseCluster] = {
        if (distClustId.length == 0) Nil
        else {
          val minDist = distClustId.map(x => x._1).min
          val removeClusterId = minDistList.filter(x => (x._1 == minDist)).map(x => (x._2, x._3)).head
          val clusterA = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._1)).head
          val clusterB = clusterCutListNoOne.filter(x => (x.clusterId == removeClusterId._2)).head
          val newCluster = EllipseClusterId(fusionEllipseCluster(clusterA.cluster, clusterB.cluster), maxId + 1)
          println("Construct fusion cluster " + (maxId + 1 + 1) + " from " + (removeClusterId._1 + 1) + " and "
            + (removeClusterId._2 + 1) + ", distance:" + minDist)
          if (distClustId.length == 3) {
            val remainClusterId = distClustId.flatMap(distIdId => List(distIdId._1, distIdId._2)).filter(id =>
              (id != removeClusterId._1 && id != removeClusterId._2)).head
            val clusterC = clusterCutListNoOne.filter(x => (x.clusterId == remainClusterId)).head
            List(ArrowEllipseCluster(clusterA, newCluster), ArrowEllipseCluster(clusterB, newCluster),
              ArrowEllipseCluster(clusterC, clusterC))
          } else {
            val newdistClustId = distClustId.filter(distIdId =>
              ((distIdId._2 != clusterA.clusterId) && (distIdId._2 != clusterB.clusterId) &&
                (distIdId._3 != clusterA.clusterId) && (distIdId._3 != clusterB.clusterId)))
            ArrowEllipseCluster(clusterA, newCluster) :: ArrowEllipseCluster(clusterB, newCluster) ::
              levelStep(newdistClustId)
          }
        }
      }

      val levelArrowCluster = levelStep(minDistList)
      val newClusterList = levelArrowCluster.map(_.target)
      levelArrowCluster ::: levelTreeEllipseCluster(newClusterList)
    }
  }

  /** Construct the minimal connected network of ellipse cluster
    *
    * @param clusterList
    * @return
    */
  def connectedCluster(clusterList: List[EllipseClusterId]): List[ArrowEllipseCluster] = {
    val clusterListNoOne = clusterList.filter(elClId => (elClId.cluster.size > 1))
    if (clusterListNoOne.length == 1) Nil
    else {
      val distList: List[(Double, Int, Int)] = (for (g <- clusterListNoOne.indices.combinations(2)) yield {
        (distEllipseCluster(clusterListNoOne(g(0)).cluster, clusterListNoOne(g(1)).cluster)
          , clusterListNoOne(g(0)).clusterId, clusterListNoOne(g(1)).clusterId)
      }).toList

      def recurConnClusterNet(cListComp: List[(EllipseClusterId, Int)], dList: List[(Double, Int, Int)]):
      List[ArrowEllipseCluster] = {
        if (cListComp.map(x => x._2).distinct.length == 1) Nil else {
          val connectDistList = dList.filter(dist => {
            val compA = cListComp.filter(cl => cl._1.clusterId == dist._2).head._2
            val compB = cListComp.filter(cl => cl._1.clusterId == dist._3).head._2
            compA != compB
          })
          val connectDist = connectDistList.filter(dist => (dist._1 == connectDistList.map(_._1).min)).head
          val clusterCompA = cListComp.filter(x => (x._1.clusterId == connectDist._2)).head
          val clusterCompB = cListComp.filter(x => (x._1.clusterId == connectDist._3)).head
          val newCListComp = cListComp.map(clComp => {
            if (clComp._2 == clusterCompB._2)
              (clComp._1,clusterCompA._2)
            else clComp
          })
          ArrowEllipseCluster(clusterCompA._1, clusterCompB._1) :: recurConnClusterNet(newCListComp, dList)
        }
      }
      recurConnClusterNet(clusterListNoOne.zipWithIndex, distList)
    }
  }
}