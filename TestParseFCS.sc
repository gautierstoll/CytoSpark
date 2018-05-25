
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

//import java.nio.ByteBuffer

val exp12FCS = new FCSParserCompact("Exp 12 T cells_Tube_001.fcs",-1000)

val kMeanExp12_0 = exp12FCS.kmeansCompensated(KMeanFCSInput(6,1000,100,0))

val kMeanExp12_Big = exp12FCS.kmeansCompensated(KMeanFCSInput(6,10000,100,50))



