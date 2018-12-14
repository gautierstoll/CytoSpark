import java.io.File
import java.nio.file.{Files, Paths}

object LinePromptData {
  def takeIntFromLine(askingPromp: String, defaultVal: Int, minVal: Int): Int = {
    val takenInt = scala.io.StdIn.readLine(askingPromp) match {
      case "" => defaultVal
      case x: String => try {
        x.toInt
      } catch {
        case _: Throwable => println("Take default: " + defaultVal); defaultVal
      }
    }
    if (takenInt < minVal) {
      println("Take min: " + minVal);
      minVal
    } else takenInt
  }

  def takeIntFromLine(askingPromp: String, defaultVal: Int, minVal: Int, maxVal: Int): Int = {
    val takenInt = scala.io.StdIn.readLine(askingPromp) match {
      case "" => defaultVal
      case x: String => try {
        x.toInt
      } catch {
        case _: Throwable => println("Take default: " + defaultVal); defaultVal
      }
    }
    takenInt match {
      case x if (x < minVal) => {
        println("Take min: " + minVal);
        minVal
      }
      case x if (x > maxVal) => {
        println("Take max: " + maxVal);
        maxVal
      }
      case x => x
    }
  }


  /** take a list of Int, separated by comma
    *
    * @param askingPromp
    * @param minVal
    * @param maxVal
    * @return
    */
  def takeListInt(askingPromp: String, minVal: Int, maxVal: Int): List[Int] = {
    scala.io.StdIn.readLine(askingPromp).
      toCharArray.filter(_ != ' ').mkString("").split(",").
      map(x => try (x.toInt) catch {
        case _: Throwable => (minVal - 1)
      }).toList.filter(x => (x <= maxVal) && (x >= minVal))
  }

  /**
    *
    * @param fcsDataFinalKMean
    * @return removed paramters starts at 1
    */
  def takeRemoveParam(fcsDataFinalKMean: FCSDataFinalKMean): List[Int] = {
    val askingListParam: String = "Remove Parameters (separated by ','): " + (for (paramAndIndex <- fcsDataFinalKMean.takenParam.zipWithIndex) yield {
      (paramAndIndex._2 + 1).toString + ": " + (try fcsDataFinalKMean.textSegmentMap("$P" + paramAndIndex._1 + "S") catch {
        case _: Throwable => fcsDataFinalKMean.textSegmentMap("$P" + paramAndIndex._1 + "N")
      })
    }).reduce(_ + ", " + _) + " "
    takeListInt(askingListParam, 1, fcsDataFinalKMean.takenParam.length)
  }

  /**
    * If file do not exist, ask for retry. If no, return null. Not used anymore
    * @return
    */
  def askFileOrNull(): String = {
    val fcsFile = scala.io.StdIn.readLine("FCS File: ")
    if (!Files.exists(Paths.get(fcsFile))) {
      println("File " + fcsFile + " do not exist")
      val retry = scala.io.StdIn.readLine("Retry? y/[n] ")
      if (retry == "Y") askFileOrNull() else null
    }
    else fcsFile
  }

  def askFileFromType(fileType: String): String = {
    val typePattern = ("\\." + fileType + "$").r
    val filesInDir = new File(".")
    val listFiles = filesInDir.listFiles.filter(file => typePattern.findAllMatchIn(file.toString).nonEmpty).map(_.toString)
    listFiles.zipWithIndex.foreach(fileInd => println((fileInd._2+1) + " " + fileInd._1))
    val fileIndex = takeIntFromLine("File number: ", 1, 1, listFiles.length)
    listFiles(fileIndex - 1)
  }

  /**
    *
    * @param fileType
    * @return
    */
  def askListFileFromType(fileType: String): List[String] = {
    val typePattern = ("\\." + fileType + "$").r
    val filesInDir = new File(".")
    val listFiles = filesInDir.listFiles.filter(file => typePattern.findAllMatchIn(file.toString).nonEmpty).map(_.toString)
    listFiles.zipWithIndex.foreach(fileInd => println((fileInd._2+1) + " " + fileInd._1))
    val fileListIndex = takeListInt("File numbers, separated by ,: ", 1, listFiles.length)
    fileListIndex.map(index => listFiles(index-1))
  }

}
