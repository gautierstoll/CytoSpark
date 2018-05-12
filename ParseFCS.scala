
class FCSParser(FCSName: String) {

  import java.io._
  //import java.nio.file.{Files, Paths}
  //import scala.util.matching.Regex
  import breeze.linalg._
  import breeze.numerics._
  import java.nio.ByteBuffer
  import org.saddle._

  val FCSFile = new String(FCSName)

  private val FCSFileBuffer = new BufferedInputStream(new FileInputStream(FCSFile))
  //private val FCSFileStr = Stream.continually(FCSFileBuffer.read).takeWhile(-1 !=).map(_.toByte)

  private var BinaryFileIndex: Int = 0;


  for (i <- (0 to 9)) yield {
    BinaryFileIndex += 1; FCSFileBuffer.read
  };
  BinaryFileIndex += 10;
  private val FirstTextSegment = (for (i <- (1 to 8)) yield {
    FCSFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  BinaryFileIndex += 8;

  for (i <- (BinaryFileIndex to 17)) yield {
    FCSFileBuffer.read
  };
  BinaryFileIndex = 18;
  private val LastTextSegment = (for (i <- (1 to 8)) yield {
    FCSFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  BinaryFileIndex += 8;

  for (i <- (BinaryFileIndex to 41)) yield {
    FCSFileBuffer.read
  };
  BinaryFileIndex = 42;
  private val FirstAnalysisSegment = (for (i <- (1 to 8)) yield {
    FCSFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  BinaryFileIndex += 8;

  for (i <- (BinaryFileIndex to 49)) yield {
    FCSFileBuffer.read
  };
  BinaryFileIndex = 50;
  private val LastAnalysisSegment = (for (i <- (1 to 8)) yield {
    FCSFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  BinaryFileIndex += 8;

  private def LengthSecondCharSep(InList: List[Byte]): Int = {
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

  private def TextSegmentMap(InList: List[Byte]): Map[String, String] = {
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


  for (i <- (BinaryFileIndex to (FirstTextSegment - 1))) yield {
    FCSFileBuffer.read
  };
  BinaryFileIndex = FirstTextSegment;

  private val FCSTextSegment = (for (i <- (BinaryFileIndex to LastTextSegment)) yield {
    FCSFileBuffer.read
  }).map(_.toByte)
  BinaryFileIndex = LastTextSegment + 1;
  val FCSTextSegmentMap = TextSegmentMap(FCSTextSegment.toList)
  println("Mode: " + FCSTextSegmentMap("$MODE"))
  println("Data type: " + FCSTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + FCSTextSegmentMap("$PAR"))
  println("Byte order: " + FCSTextSegmentMap("$BYTEORD"))

  val FirstDataSegment = FCSTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  val LastDataSegment = FCSTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  val NbPar = FCSTextSegmentMap("$PAR").toInt
  val NbEvent = FCSTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  //val NbEvent: Int = 5
  val BittoFloat = (1 to NbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => FCSTextSegmentMap(x).toInt).toList
  for (i <- (BinaryFileIndex to (FirstDataSegment - 1))) yield {
    FCSFileBuffer.read
  }
  BinaryFileIndex = FirstDataSegment
  private val dataFCSList = {
    for (indexFCS <- (0 to (NbEvent * BittoFloat.length - 1))) yield {
      if (BittoFloat(indexFCS - (indexFCS / BittoFloat.length)*BittoFloat.length) == 32) {
        BinaryFileIndex += 4
        ByteBuffer.wrap((1 to 4).map(x => FCSFileBuffer.read.toByte).toArray).getFloat.toDouble
      }
      else if (BittoFloat(indexFCS - (indexFCS / BittoFloat.length)*BittoFloat.length) == 64) {
        BinaryFileIndex += 8
        ByteBuffer.wrap((1 to 8).map(x => FCSFileBuffer.read.toByte).toArray).getDouble
      }
      else {
        0.0
      }
    }
  }


  def getMatrixFCS: DenseMatrix[Double] = {
    var FCSMatrix = new DenseMatrix[Double](NbEvent, BittoFloat.length)
    for (rowFCS <- (0 to (NbEvent - 1)); colFCS <- (0 to (BittoFloat.length - 1))) {
      FCSMatrix(rowFCS, colFCS) = dataFCSList(colFCS + rowFCS * (BittoFloat.length))
    }
    return (FCSMatrix)
  }

  def getMatFCS: Mat[Double] = {
    val fcsMat: Mat[Double] = Mat(BittoFloat.length, NbEvent, dataFCSList.toArray)
    fcsMat
  }
}
