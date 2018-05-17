
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import java.nio.ByteBuffer
import org.apache.spark.sql.types._


class FCSParserSpark(fcsNameInput: String, minValCytInput: Double) {
  def textSegmentMap(inList: List[Byte]): Map[String, String] = {
    def lengthSecondCharSep(inList: List[Byte]): Int = {
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

  private val offsetByteText: (Int, Int, Int) = (10, 17, 25)
  private val offsetByteAnalysis: (Int, Int, Int) = (42, 49, 57)
  val fcsFile = new String(fcsNameInput)
  val minValCyt = minValCytInput


  val fileList = TestSessSpark.sparkContext.binaryRecords(fcsFile, 1)
  val firstTextSegment = fileList.take(offsetByteText._2+1 ).drop(offsetByteText._1).
    toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
  val lastTextSegment = fileList.take(offsetByteText._3+1 ).drop(offsetByteText._2+1).
    toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
  val firstAnalysisSegment = fileList.take(offsetByteAnalysis._2+1).drop(offsetByteAnalysis._1).
    toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
  val lastAnalysisSegment = fileList.take(offsetByteAnalysis._3+1).drop(offsetByteAnalysis._2+1).
    toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
  val fcsTextSegment = fileList.take(lastTextSegment + 1).drop(firstTextSegment).toList.map(_.head)

  val fcsTextSegmentMap = textSegmentMap(fcsTextSegment)
  println("Mode: " + fcsTextSegmentMap("$MODE"))
  println("Data type: " + fcsTextSegmentMap("$DATATYPE"))
  println("Number of chanels: " + fcsTextSegmentMap("$PAR"))
  println("Byte order: " + fcsTextSegmentMap("$BYTEORD"))

  val firstDataSegment = fcsTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
  val lastDataSegment = fcsTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
  //val FCSDataStr = FCSFileStr.drop(firstDataSegment).take(lastDataSegment - firstDataSegment + 1)
  val nbPar = fcsTextSegmentMap("$PAR").toInt
  val nbEvent = fcsTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
  
  val bitToFloat = (1 to nbPar).
    map(x => "$P".concat(x.toString).concat("B")).map(x => fcsTextSegmentMap(x).toInt).toList

  def fcsArrayDoublefromFCS(fcsLine: List[Byte], bit4Float: List[Int]): List[Double] = {
    def byteAggregate(listOfBit: List[Int], Index: Int = 0): List[Int] = listOfBit match {
      case n :: Nil => List.fill(n / 8)(Index)
      case n :: xx => List.fill(n / 8)(Index) ::: byteAggregate(xx, Index + 1)
      case Nil => sys.error("Error in parsing Bytes according to TextSegment")
    }

    def byteToDoubleSizeDependant(arrayBytes: Array[Byte]): Double = arrayBytes.length match {
      case 4 => ByteBuffer.wrap(arrayBytes).getFloat
      case 8 => ByteBuffer.wrap(arrayBytes).getDouble
    }

    (0 to (bit4Float.length - 1)).toList.
      map(x => fcsLine.zip(byteAggregate(bitToFloat)).filter(_._2 == x).map(y => y._1)).
      map(z => byteToDoubleSizeDependant(z.toArray))
  }
  val dataList = fileList.zipWithIndex().filter(x => ((x._2 >= firstDataSegment) && (x._2 <= lastDataSegment))).
    map(y => ((y._2 - firstDataSegment) / (bitToFloat.sum / 8), y._1.head)).groupByKey.
    map(x => (x._1, fcsArrayDoublefromFCS(x._2.toList, bitToFloat))
}