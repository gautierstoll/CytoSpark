import java.io._
//import java.nio.file.{Files, Paths}
//import scala.util.matching.Regex
import breeze.linalg._
import breeze.numerics._
import java.nio.ByteBuffer

//val fileBuffer = Files.readAllBytes(Paths.
//  get("/Users/gstoll/Documents/" +
//    "CytoSparkMac/Miltenyi Biotec - MACSQuant Analyzer.fcs")).toList
val FCSFileBuffer = new BufferedInputStream(new FileInputStream("Specimen_001_Tube_008 Tumor8.fcs"))
//val FCSFileStr = Stream.continually(FCSFileBuffer.read).takeWhile(-1 !=).map(_.toByte)
var BinaryFileIndex:Int = 0;


for (i <- (0 to 9)) yield {BinaryFileIndex +=1;FCSFileBuffer.read}; BinaryFileIndex += 10 ;
val FirstTextSegment = (for (i <- (1 to 8)) yield {FCSFileBuffer.read}).
  map(_.toChar).filter(_ != ' ').mkString("").toInt; BinaryFileIndex += 8 ;

for (i <- (BinaryFileIndex to 17)) yield {FCSFileBuffer.read}; BinaryFileIndex = 18 ;
val LastTextSegment = (for (i <- (1 to 8)) yield {FCSFileBuffer.read}).
  map(_.toChar).filter(_ != ' ').mkString("").toInt; BinaryFileIndex += 8 ;
//val FirstDataSegment = FCSFileStr.drop(26).take(8).map(_.toChar).filter(_ != ' ').mkString("").toInt
//val LastDataSegment = FCSFileStr.drop(34).take(8).map(_.toChar).filter(_ != ' ').mkString("").toInt

for (i <- (BinaryFileIndex to 41)) yield {FCSFileBuffer.read}; BinaryFileIndex = 42 ;
val FirstAnalysisSegment = (for (i <- (1 to 8)) yield {FCSFileBuffer.read}).
  map(_.toChar).filter(_ != ' ').mkString("").toInt; BinaryFileIndex += 8 ;

for (i <- (BinaryFileIndex to 49)) yield {FCSFileBuffer.read}; BinaryFileIndex = 50 ;
val LastAnalysisSegment = (for (i <- (1 to 8)) yield {FCSFileBuffer.read}).
  map(_.toChar).filter(_ != ' ').mkString("").toInt; BinaryFileIndex += 8 ;

def LengthSecondCharSep(InList: List[Byte]): Int = {
  def DropUntilSinglSep(SepByte: Byte, Offset: Int, CharList: List[Byte]): Int = {
    var NewOffset = Offset + CharList.drop(Offset).takeWhile(_ != SepByte).length
    CharList.drop(NewOffset) match {
      case SepByte :: Nil => NewOffset
      // two separators is not a separator
      case SepByte :: SepByte :: yy => DropUntilSinglSep(SepByte, NewOffset + 2, CharList)
      case SepByte :: yy => NewOffset
      case yy => {
        sys.error("Error in Parsing text segment");
        0
      }
    }
  }

  DropUntilSinglSep(InList.head, 1, InList)
}

def TextSegmentMap(InList: List[Byte]): Map[String, String] = {
  val KeyLength = LengthSecondCharSep(InList) - 1
  val ValLength = LengthSecondCharSep(InList.drop(KeyLength + 1)) - 1
  if (InList.length <= (KeyLength + ValLength + 3)) {
    Map(InList.drop(1).take(KeyLength).map(_.toChar).mkString("") ->
      InList.drop(1 + KeyLength + 1).take(ValLength).map(_.toChar).mkString(""))
  }
  else {
    Map(InList.drop(1).take(KeyLength).map(_.toChar).mkString("") ->
      InList.drop(1 + KeyLength + 1).take(ValLength).map(_.toChar).mkString("")) ++
      TextSegmentMap(InList.drop(1 + KeyLength + 1 + ValLength))
  }
}


for (i <- (BinaryFileIndex to (FirstTextSegment-1))) yield {FCSFileBuffer.read}; BinaryFileIndex =  FirstTextSegment;

val FCSTextSegment = (for (i <- (BinaryFileIndex to LastTextSegment)) yield {FCSFileBuffer.read}).map(_.toByte)
BinaryFileIndex = LastTextSegment+1;
val FCSTextSegmentMap = TextSegmentMap(FCSTextSegment.toList)
println("Mode: " + FCSTextSegmentMap("$MODE"))
println("Data type: " + FCSTextSegmentMap("$DATATYPE"))
println("Number of chanels: " + FCSTextSegmentMap("$PAR"))
println("Byte order: " + FCSTextSegmentMap("$BYTEORD"))

val FirstDataSegment = FCSTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
val LastDataSegment = FCSTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
val FCSDataStr = FCSFileStr.drop(FirstDataSegment).take(LastDataSegment - FirstDataSegment + 1)
val NbPar = FCSTextSegmentMap("$PAR").toInt
//val NbEvent = FCSTextSegmentMap("$TOT").toInt
val NbEvent: Int = 100
val BittoFloat = (1 to NbPar).
  map(x => "$P".concat(x.toString).concat("B")).map(x => FCSTextSegmentMap(x).toInt).toList

//var FCSMatrixEvents: DenseMatrix[Double] = new DenseMatrix[Double](NbEvent, NbPar)

//def GetDoubleFCSListFromLines(FCSEvents4Array: Int, BittoFloat4Array: List[Int],
//                              FCSByteStr4Array: Stream[Byte]): List[Double] = {
//  def GetDoubleFCSLine(NbByteList: List[Int], FCSByteStr: Stream[Byte]): List[Double] =
//    NbByteList match {
//      case Nil => Nil
//      case 4 :: xxByteTail =>
//        (ByteBuffer.wrap(FCSByteStr.take(4).toArray).getFloat) :: GetDoubleFCSLine(xxByteTail, FCSByteStr.drop(4))
//      case 8 :: xxByteTail =>
//        (ByteBuffer.wrap(FCSByteStr.take(8).toArray).getDouble) :: GetDoubleFCSLine(xxByteTail, FCSByteStr.drop(8))
//      case x :: xx => sys.error("Wrong Byte List")
//    }
//
//  if (FCSEvents4Array > 0) {
//    GetDoubleFCSLine(BittoFloat4Array.map(_ / 8), FCSByteStr4Array) :::
//      GetDoubleFCSListFromLines(FCSEvents4Array - 1, BittoFloat4Array, FCSByteStr4Array.drop(BittoFloat4Array.map(_ / 8).sum))
//  }
//  else {
//    Nil
//  }
//}
//



