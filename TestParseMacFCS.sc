import java.io._
//import java.nio.file.{Files, Paths}
//import scala.util.matching.Regex
import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer
import org.saddle._
import org.saddle.linalg._

import org.nspl._
import org.nspl.saddle._
import org.nspl.data._
import stat.sparse._
import stat.kmeans._
import awtrenderer._
import stat._

def testKplot(data: SMat, res: KMeansResult, max: Int) = {

  val projections = 0 until max combinations (2) map { g =>
    val c1 = g(0)
    val c2 = g(1)
    val col1 = data.map(s => s.values.first(c1).getOrElse(0d)).toVec
    val col2 = data.map(s => s.values.first(c2).getOrElse(0d)).toVec
    xyplot(
      Mat(col1, col2, res.clusters.map(_.toDouble)) -> point(
        labelText = false,
        color = DiscreteColors(res.clusters.length)))()
  }

  sequence(projections.toList, TableLayout(4))

}




