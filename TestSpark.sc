import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import java.nio.ByteBuffer
import org.apache.spark.sql.types._

val testSessSpark = SparkSession.builder().
  appName("Test Spark Session").config("spark.master", "local").getOrCreate();
testSessSpark.conf.set("spark.executor.memory", "10g")
testSessSpark.conf.set("spark.driver.memory", "2g")
testSessSpark.conf.set("spark.cores.max", "6")

val fileList = testSessSpark.sparkContext.binaryRecords("Exp 12 T cells_Tube_001.fcs", 1)
val firstTextSegment = fileList.take(18).drop(10).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val lastTextSegment = fileList.take(26).drop(18).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val firstAnalysisSegment = fileList.take(50).drop(42).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val lastAnalysisSegment = fileList.take(58).drop(50).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val fcsTextSegment = fileList.take(lastTextSegment + 1).drop(firstTextSegment).toList.map(_.head)

//TestSessSpark.stop

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

val fcsTextSegmentMap = textSegmentMap(fcsTextSegment)
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

def fcsArrayDoublefromFCS(FCSLine: List[Byte], Bit4Float: List[Int]): List[Double] = {
  def byteAggregate(ListOfBit: List[Int], Index: Int = 0): List[Int] = ListOfBit match {
    case n :: Nil => List.fill(n / 8)(Index)
    case n :: xx => List.fill(n / 8)(Index) ::: byteAggregate(xx, Index + 1)
    case Nil => sys.error("Error in parsing Bytes according to TextSegment")
  }

  def byteToDoubleSizeDependant(ArrayBytes: Array[Byte]): Double = ArrayBytes.length match {
    case 4 => ByteBuffer.wrap(ArrayBytes).getFloat
    case 8 => ByteBuffer.wrap(ArrayBytes).getDouble
  }
  (0 to (Bit4Float.length - 1)).toList.
    map(x => FCSLine.zip(byteAggregate(bittoFloat)).filter(_._2 == x).map(y => y._1)).
    map(z => byteToDoubleSizeDependant(z.toArray))
}
val dataList = fileList.zipWithIndex().filter(x => ((x._2 >= firstDataSegment) && (x._2 <= lastDataSegment))).
// val DataList = fileList.zipWithIndex().filter(x => ((x._2 >= firstDataSegment) && (x._2 <= (firstDataSegment+3215)))).
  map(y => ((y._2-firstDataSegment) /(bittoFloat.sum/8),y._1.head)).groupByKey.
  map(x => (x._1,fcsArrayDoublefromFCS(x._2.toList, bittoFloat))) //keep key because groupByKey shuffles