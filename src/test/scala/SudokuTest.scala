import java.io.File

import model.{FileUtils, Model}
import org.junit.jupiter.api._

class SudokuTest {

  @Test
  @Order(1)
  def loadingFileTest(): Unit = {
    val fileName : String = "file_1.txt"
    println("--- FILE LOADING")

    // Adequate type?
    assert(FileUtils.isFileAppropriate(fileName), "File type isn't appropriate!")

    // Is loading successful?
    val file : File = new File(FileUtils.PATH_TO_EXAMPLES + "\\" + fileName)
    Model.loadFile(file, 0, isTesting = true)
    assert(Model.convertCellDataToString().replace("\n", "").equalsIgnoreCase(FileUtils.getFileContent(file)))

    println("- " + fileName + " loaded!")
  }

  @Test
  @Order(2)
  def settingAndDeletingValues(): Unit = {
    println("--- SET AND DELETE VALUE")

    Model.setCellValue(0, 0, 1)
    assert(!(Model.getData(0)(0).info == 1))
    println("- Setting on (0,0) failed because element is original!")

    Model.setCellValue(0, 1, 1)
    assert(Model.getData(0)(1).info == 1)
    println("- Setting on (0,1), passed!")

    Model.removeCellValue(0, 1, isTesting = true)
    assert(Model.getData(0)(1).info == -1)
    println("- Deleting from (0,1), passed!")
  }

  @Test
  @Order(3)
  def complementAndTransposeTest(): Unit = {
    println("--- COMPLEMENT AND TRANSPOSE")
    Model.transposeMatrix(true)
    Model.transposeMatrix(true)

    Model.complementMatrix(true)
    Model.complementMatrix(true)

    val file : File = new File(FileUtils.PATH_TO_EXAMPLES + "\\file_1.txt")
    Model.loadFile(file, 0, isTesting = true)
    assert(Model.convertCellDataToString().replace("\n", "").equalsIgnoreCase(FileUtils.getFileContent(file)))

    println("- Table is complemented and then transposed!")
  }

  @Test
  @Order(4)
  def canBeSolvedCheck(): Unit = {
    println("--- SOLVING CHECK")
    assert(Model.isSudokuValid(Model.getData))
    println("- Table can be solved!")
  }
}
