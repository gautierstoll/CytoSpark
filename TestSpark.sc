import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import java.nio.ByteBuffer
import org.apache.spark.sql.types._

val TestSessSpark = SparkSession.builder().
  appName("Test Spark Session").config("spark.master", "local").getOrCreate();
TestSessSpark.conf.set("spark.executor.memory", "10g")
TestSessSpark.conf.set("spark.driver.memory", "2g")
TestSessSpark.conf.set("spark.cores.max", "6")

val FileList = TestSessSpark.sparkContext.binaryRecords("Exp 12 T cells_Tube_001.fcs", 1)
val FirstTextSegment = FileList.take(18).drop(10).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val LastTextSegment = FileList.take(26).drop(18).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val FirstAnalysisSegment = FileList.take(50).drop(42).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val LastAnalysisSegment = FileList.take(58).drop(50).toList.map(_.head.toChar).filter(_ != ' ').mkString("").toInt
val FCSTextSegment = FileList.take(LastTextSegment + 1).drop(FirstTextSegment).toList.map(_.head)

//TestSessSpark.stop

def TextSegmentMap(InList: List[Byte]): Map[String, String] = {
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

val FCSTextSegmentMap = TextSegmentMap(FCSTextSegment)
println("Mode: " + FCSTextSegmentMap("$MODE"))
println("Data type: " + FCSTextSegmentMap("$DATATYPE"))
println("Number of chanels: " + FCSTextSegmentMap("$PAR"))
println("Byte order: " + FCSTextSegmentMap("$BYTEORD"))

val FirstDataSegment = FCSTextSegmentMap("$BEGINDATA").toList.filter(_ != ' ').mkString("").toInt
val LastDataSegment = FCSTextSegmentMap("$ENDDATA").toList.filter(_ != ' ').mkString("").toInt
//val FCSDataStr = FCSFileStr.drop(FirstDataSegment).take(LastDataSegment - FirstDataSegment + 1)
val NbPar = FCSTextSegmentMap("$PAR").toInt
val NbEvent = FCSTextSegmentMap("$TOT").toArray.filter(_ != ' ').mkString("").toInt
//val NbEvent: Int = 100000
val BittoFloat = (1 to NbPar).
  map(x => "$P".concat(x.toString).concat("B")).map(x => FCSTextSegmentMap(x).toInt).toList

def FCSArrayDoublefromFCS(FCSLine: List[Byte], Bit4Float: List[Int]): List[Double] = {
  def ByteAggregate(ListOfBit: List[Int], Index: Int = 0): List[Int] = ListOfBit match {
    case n :: Nil => List.fill(n / 8)(Index)
    case n :: xx => List.fill(n / 8)(Index) ::: ByteAggregate(xx, Index + 1)
    case Nil => sys.error("Error in parsing Bytes according to TextSegment")
  }

  def ByteToDoubleSizeDependant(ArrayBytes: Array[Byte]): Double = ArrayBytes.length match {
    case 4 => ByteBuffer.wrap(ArrayBytes).getFloat
    case 8 => ByteBuffer.wrap(ArrayBytes).getDouble
  }
  (0 to (Bit4Float.length - 1)).toList.
    map(x => FCSLine.zip(ByteAggregate(BittoFloat)).filter(_._2 == x).map(y => y._1)).
    map(z => ByteToDoubleSizeDependant(z.toArray))
}
 val DataList = FileList.zipWithIndex().filter(x => ((x._2 >= FirstDataSegment) && (x._2 <= LastDataSegment))).
 //val DataList = FileList.zipWithIndex().filter(x => ((x._2 >= FirstDataSegment) && (x._2 <= (FirstDataSegment+103)))).
  map(y => ((y._2-FirstDataSegment) /(BittoFloat.sum/8),y._1.head)).groupByKey.
  map(x => (x._1,FCSArrayDoublefromFCS(x._2.toList, BittoFloat))
//val DataDF = TestSessSpark.createDataFrame(DataList)

