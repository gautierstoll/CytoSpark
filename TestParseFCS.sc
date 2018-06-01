
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
import scala.collection.parallel.mutable._

//import java.nio.ByteBuffer

//val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)
//
//val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,0))
//
//val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6,10000,100,50))

val inputExp12 =
  FCSInputFull("Exp 12 T cells_Tube_001.fcs",
    List((5,true,0.0), (6,true,0.0), (7,true,0.0), (8,true,0.0), (9,true,0.0), (10,true,0.0), (11,true,0.0)),10000)

val dataExp12 = new FCSParserFull(inputExp12)
val kmeanExp12 = dataExp12.kmeansFCS(KMeanFCSInput(7,10000,100,0))
