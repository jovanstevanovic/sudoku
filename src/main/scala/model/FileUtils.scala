package model

import java.io._
import java.util.Scanner

import model.Model.DataUnit

object FileUtils {
  // Constants
  val PATH_TO_EXAMPLES : String = System.getProperty("user.dir") + "\\src\\examples"
  val PATH_TO_OPERATIONS : String = System.getProperty("user.dir") + "\\src\\operations"
  val PATH_TO_SOLUTIONS : String = System.getProperty("user.dir") + "\\src\\solutions"

  // Object's fields
  val folderWithExamples = new File(PATH_TO_EXAMPLES)
  val folderWithOperations = new File(PATH_TO_OPERATIONS)
  val folderWithSolutions = new File(PATH_TO_SOLUTIONS)

  var nextIdExamples : Int = readAllFilesFromDirectory(0).length
  var nextIdOperations : Int = readAllFilesFromDirectory(1).length
  var nextIdSolutions : Int = readAllFilesFromDirectory(2).length

  // mode == 0 - examples ; mode == 1 - operations ; mode == 2 - solutions
  def readAllFilesFromDirectory(mode : Int) : Array[String] = {
    if (mode == 0) {
      for {file <- folderWithExamples.listFiles if file.getName.endsWith(".txt")} yield file.getName
    } else {
      if (mode == 1){
        for {file <- folderWithOperations.listFiles if file.getName.endsWith(".txt")} yield file.getName
      } else {
        for {file <- folderWithSolutions.listFiles if file.getName.endsWith(".txt")} yield file.getName
      }
    }
  }

  def isFileAppropriate(targetFileName : String) : Boolean = {
    targetFileName.endsWith(".txt")
  }

  // mode == 0 - examples ; mode == 1 - operations ; mode == 2 - solutions
  def createNewFile(fileText : String, mode : Int) : Unit = {
    var nextId : Int = 0
    var path : String = ""

    mode match {
      case 0 =>
        nextIdExamples += 1
        nextId = nextIdExamples
        path = PATH_TO_EXAMPLES
      case 1 =>
        nextIdOperations += 1
        nextId = nextIdOperations
        path = PATH_TO_OPERATIONS
      case 2 =>
        nextIdSolutions += 1
        nextId = nextIdSolutions
        path = PATH_TO_SOLUTIONS
    }

    val newFile = new File(path + "\\file_" + nextId + ".txt")
    try {
      newFile.createNewFile()
      writeInFile(newFile, fileText)
    } catch {
      case error: IOException => error.printStackTrace()
    }
  }

  def writeInFile(file : File, content : String): Unit = {
    try {
      val myWriter = new FileWriter(file)
      myWriter.write(content)
      myWriter.close()
    } catch {
      case error: IOException => error.printStackTrace()
    }
  }

  // mode == 0 - game is running ; mode == 1 - table is creating
  def readFile(file : File, data : Array[Array[DataUnit]], mode : Int): (Int, Int) = {
    var internalRowCounter : Int = 0
    var courser_x = -1
    var courser_y = -1

    def processLine(line : String) : Unit = {
      var internalColumnCounter : Int = 0
      for (singleChar <- line) {
        data(internalRowCounter)(internalColumnCounter).original = false
        if (singleChar == '-') {
          data(internalRowCounter)(internalColumnCounter).info = -1
        }
        if (singleChar == 'P') {
          courser_x = internalRowCounter
          courser_y = internalColumnCounter
          data(internalRowCounter)(internalColumnCounter).info = -1
        }
        if(singleChar.isDigit) {
          data(internalRowCounter)(internalColumnCounter).info = singleChar.toInt - '0'.toInt
          if (mode == 0)
            data(internalRowCounter)(internalColumnCounter).original = true
        }
        internalColumnCounter += 1
      }
      internalRowCounter += 1
    }

    try {
      val myReader = new Scanner(file)
      while (myReader.hasNextLine) {
        val line = myReader.nextLine
        processLine(line)
      }
      myReader.close()
    } catch {
        case e: FileNotFoundException =>
          e.printStackTrace()
    }
    (courser_x, courser_y)
  }

  def getFileContent(file : File) : String = {
    var line : String = ""
    try {
      val myReader = new Scanner(file)
      while (myReader.hasNextLine) {
        line += myReader.nextLine
      }
      myReader.close()
    } catch {
        case e: FileNotFoundException =>
          e.printStackTrace()
    }
    line
  }
}
