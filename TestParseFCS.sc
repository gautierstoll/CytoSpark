import java.nio._
import Array._
import breeze.linalg._
import breeze.numerics._
//import java.nio.ByteBuffer

val tmpPath = file.Paths.get("/mnt/data/CytoSpark/Exp 12 T cells_Tube_001.fcs")
val fileBuffer = file.Files.readAllBytes(tmpPath)

val TextSegment = (fileBuffer.drop(10).take(8).
  map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt,
  fileBuffer.drop(18).take(8).map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt)

val DataSegment = (fileBuffer.drop(26).take(8).
  map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt,
  fileBuffer.drop(34).take(8).map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt)
val AnalysisSegment = (fileBuffer.drop(26).take(8).
  map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt,
  fileBuffer.drop(42).take(50).map(x => x.toChar).dropWhile(x => x == ' ').mkString("").toInt)

def parseTextSegment(textSeg: Array[Byte]): scala.collection.mutable.Map[String, String] = {
  val Sep = textSeg(0);
  val mKey: String = textSeg.drop(1).takeWhile(x => (x != Sep)).map(y => y.toChar).mkString("");
  val mValue: String = textSeg.drop(1).dropWhile(x => (x != Sep)).drop(1).
    takeWhile(x => (x != Sep)).map(y => y.toChar).mkString("");
  if (textSeg.filter(x => (x == textSeg(0))).length == 3) {
    scala.collection.mutable.Map(mKey -> mValue)
  }
  else {
    parseTextSegment(textSeg.drop(1).dropWhile(x => (x != Sep)).drop(1).dropWhile(x => (x != Sep))) + (mKey -> mValue)
  }
}
val MapTextSegment = parseTextSegment(fileBuffer.drop(TextSegment._1).take(TextSegment._2 - TextSegment._2 + 1))
val NbPar = MapTextSegment("$PAR").toInt
val NbEvent = MapTextSegment("$PAR").toInt
val BittoFloat = (1 to NbPar).toArray.map(x => "$P".concat(x.toString).concat("B")).map(x => MapTextSegment(x).toInt)

def parseDataFloat(DataSeg: Array[Byte],BitSeq : Array[Int]) : Array[Float] = {
  val CumulByteSeq = BitSeq.foldLeft(List(0))((x,y) => (x :+ (x.last+y/8))).drop(1)
  if (DataSeg.length <= CumulByteSeq.last)
    {
      CumulByteSeq.map(x => ByteBuffer.wrap(DataSeg.drop((x-4)).take(4)).getFloat).toArray
    }
  else
    {
      concat(CumulByteSeq.map(x => ByteBuffer.wrap(DataSeg.drop((x-4)).take(4)).getFloat).toArray,
        parseDataFloat(DataSeg.drop(CumulByteSeq.last),BitSeq))
    }
}



//val tmpNumbers=BittoFloat.foldLeft(List(0))((x,y) => (x :+ (x(x.length-1)+y))).drop(1).
//  map(x => ByteBuffer.wrap(fileBuffer.drop(TextSegment+(x/8)).take(4)).getFloat)

