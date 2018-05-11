
class FCSParser(fcsName: String) {

  import java.io._
  //import java.nio.file.{Files, Paths}
  //import scala.util.matching.Regex
  import breeze.linalg._
  import breeze.numerics._
  import java.nio.ByteBuffer

  val fcsFile = new String(fcsName)

  private val fcsFileBuffer = new BufferedInputStream(new FileInputStream(fcsFile))
  //private val FCSFileStr = Stream.continually(FCSFileBuffer.read).takeWhile(-1 !=).map(_.toByte)

  private var binaryFileIndex: Int = 0;


  for (i <- (0 to 9)) yield {
    binaryFileIndex += 1; fcsFileBuffer.read
  };
  binaryFileIndex += 10;
  private val firstTextSegment = (for (i <- (1 to 8)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex += 8;

  for (i <- (binaryFileIndex to 17)) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = 18;
  private val lastTextSegment = (for (i <- (1 to 8)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex += 8;
  //val FirstDataSegment = FCSFileStr.drop(26).take(8).map(_.toChar).filter(_ != ' ').mkString("").toInt
  //val LastDataSegment = FCSFileStr.drop(34).take(8).map(_.toChar).filter(_ != ' ').mkString("").toInt

  for (i <- (binaryFileIndex to 41)) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = 42;
  private val firstAnalysisSegment = (for (i <- (1 to 8)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex += 8;

  for (i <- (binaryFileIndex to 49)) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = 50;
  private val lastAnalysisSegment = (for (i <- (1 to 8)) yield {
    fcsFileBuffer.read
  }).
    map(_.toChar).filter(_ != ' ').mkString("").toInt;
  binaryFileIndex += 8;

  private def lengthSecondCharSep(inList: List[Byte]): Int = {
    def dropUntilSinglSep(SepByte: Byte, offset: Int, charList: List[Byte]): Int = {
      var newOffset = offset + charList.drop(offset).takeWhile(_ != SepByte).length
      charList.drop(newOffset) match {
        case SepByte :: Nil => newOffset
        // two separators is not a separator
        case SepByte :: SepByte :: yy => dropUntilSinglSep(SepByte, newOffset + 2, charList)
        case SepByte :: yy => newOffset
        case yy => {
          sys.error("Error in Parsing text segment");
          0
        }
      }
    }

    dropUntilSinglSep(inList.head, 1, inList)
  }

  private def textSegmentMap(inList: List[Byte]): Map[String, String] = {
    val keyLength = lengthSecondCharSep(inList) - 1
    val valLength = lengthSecondCharSep(inList.drop(keyLength + 1)) - 1
    if (inList.length <= (keyLength + valLength + 3)) {
      Map(inList.drop(1).take(keyLength).map(_.toChar).mkString("") ->
        inList.drop(1 + keyLength + 1).take(valLength).map(_.toChar).mkString(""))
    }
    else {
      Map(inList.drop(1).take(keyLength).map(_.toChar).mkString("") ->
        inList.drop(1 + keyLength + 1).take(valLength).map(_.toChar).mkString("")) ++
        textSegmentMap(inList.drop(1 + keyLength + 1 + valLength))
    }
  }


  for (i <- (binaryFileIndex to (firstTextSegment - 1))) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = firstTextSegment;

  private val fcsTextSegment = (for (i <- (binaryFileIndex to lastTextSegment)) yield {
    fcsFileBuffer.read
  }).map(_.toByte)
  binaryFileIndex = lastTextSegment + 1;
  val fcsTextSegmentMap = textSegmentMap(fcsTextSegment.toList)
  println("Mode: " + fcsTextSegmentMap("$MODE"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Byte order: " + fcsTextSegmentMap("$BYTEORD"))

  val firstDataSegment = fcsTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  val lastDataSegment = fcsTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  //val FCSDataStr = FCSFileStr.drop(FirstDataSegment).take(LastDataSegment - FirstDataSegment + 1)
  val nbPar = fcsTextSegmentMap("$PAR").toInt
  val nbEvent = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  //val NbEvent: Int = 100000
  val bittoFloat = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList

  private def getMatrixFCS(nbEvents4Matrix: Int = nbEvent, bittoFloat4Matrix: List[Int] = bittoFloat,
                           fcsByteStr: BufferedInputStream): DenseMatrix[Double] = {
    var fcsMatrix = new DenseMatrix[Double](nbEvents4Matrix, bittoFloat4Matrix.length)
    for (Row <- (0 to (nbEvents4Matrix - 1)); Col <- (0 to (bittoFloat4Matrix.length - 1))) yield {
      if (bittoFloat(Col) == 32) {
        fcsMatrix(Row, Col) = ByteBuffer.wrap((1 to 4).map(x => fcsByteStr.read.toByte).toArray).getFloat;
        binaryFileIndex += 4;
      }
      else if (bittoFloat(Col) == 64) {
        fcsMatrix(Row, Col) = ByteBuffer.wrap((1 to 8).map(x => fcsByteStr.read.toByte).toArray).getDouble
        binaryFileIndex += 8;
      }
      else {
        sys.error("Wrong Byte List")
      }
      if (Col == 0) {
        println("Row: " + Row)
      }
    }
    return (fcsMatrix)
  }

  for (i <- (binaryFileIndex to (firstDataSegment - 1))) yield {
    fcsFileBuffer.read
  };
  binaryFileIndex = firstDataSegment
  val fcsMatrix = getMatrixFCS(fcsByteStr = fcsFileBuffer)
}