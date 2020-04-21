package model

import java.awt.Font
import java.io.File

import javax.swing.JTextField

import scala.collection.mutable

object Model {
  // Constants
  val PLAYING_GAME : Int = 0
  val EDITING_TABLE : Int = 1
  val DIMENSION : Int = 9

  // Object's fields
  private var data = Array.ofDim[DataUnit](DIMENSION, DIMENSION)
  private var activeCells = Array.ofDim[JTextField](DIMENSION, DIMENSION)
  private var playGameCells = Array.ofDim[JTextField](DIMENSION, DIMENSION)
  private var editTableCells = Array.ofDim[JTextField](DIMENSION, DIMENSION)

  private var cursor_x : Int = -1
  private var cursor_y : Int = -1

  val nonSelectedCellFont : Font = new Font("SansSerif", Font.ITALIC, 14)
  val selectedCellFont : Font = new Font("SansSerif", Font.ITALIC + Font.BOLD, 14)

  private val s = Math.sqrt(DIMENSION).toInt

  class DataUnit(var info : Int, var original : Boolean)

  def getData: Array[Array[DataUnit]] = data

  private def initializeData() : Unit = {
    for(i <- 0 until DIMENSION; j <- 0 until DIMENSION)
      data(i)(j) = new DataUnit(-1, false)
  }

  def updateDate(n : Int, m : Int, value : Int) : Unit = {
    data(n)(m).info = value
  }

  // mode == 0 - play game mode ; mode == 1 - edit sudoku table
  def loadFile(file : File, mode: Int) : Unit = {
    if (mode == 0) {
      this.activeCells = playGameCells
    } else {
      this.activeCells = editTableCells
    }

    val courser = FileUtils.readFile(file, data, mode)
    cursor_x = courser._1
    cursor_y = courser._2

    refreshTableState()
    refreshCursorState()
  }

  def refreshTableState() : Unit = {
    for(i <- 0 until Model.DIMENSION; j <- 0 until Model.DIMENSION) {
      if (data(i)(j).info != -1)
        activeCells(i)(j).setText(data(i)(j).info.toString)
      else
        activeCells(i)(j).setText("")
      activeCells(i)(j).setFocusable(!data(i)(j).original)
    }
  }

  def restorePreviousCellState() : Unit = {
    activeCells(cursor_x)(cursor_y).setFont(nonSelectedCellFont)
  }

  def refreshCursorState() : Unit = {
    activeCells(cursor_x)(cursor_y).setFont(selectedCellFont)
    activeCells(cursor_x)(cursor_y).requestFocus()
  }

  // mode == 0 - play game mode ; mode == 1 - edit sudoku table
  def initCellsInModel(cells : Array[Array[JTextField]], mode : Int): Unit = {
    if (mode == 0) {
      this.playGameCells = cells
    } else {
      this.editTableCells = cells
    }
  }

  def transposeMatrix() : Unit = {
    for(i <- 0 until DIMENSION; j <- i until DIMENSION) {
      val temp = data(i)(j).info
      data(i)(j).info = data(j)(i).info
      data(j)(i).info = temp
    }

    refreshTableState()
    refreshCursorState()
  }

  def complementMatrix() : Unit = {
    for(i <- 0 until DIMENSION; j <- 0 until DIMENSION if data(i)(j).info != -1)
      data(i)(j).info = DIMENSION - data(i)(j).info + 1

    refreshTableState()
    refreshCursorState()
  }

  def filterRowAndColumn() : Unit = {
    val cellValue : Int = data(cursor_x)(cursor_y).info
    for(i <- 0 until DIMENSION if data(i)(cursor_y).info == cellValue && i != cursor_x)
      data(i)(cursor_y).info = -1
    for(j <- 0 until DIMENSION if data(cursor_x)(j).info == cellValue && j != cursor_y)
      data(cursor_x)(j).info = -1

    refreshTableState()
    refreshCursorState()
  }

  def filterSubMatrix() : Unit = {
    val cellValue : Int = data(cursor_x)(cursor_y).info
    val new_n = cursor_x / 3 * 3
    val new_m = cursor_y / 3 * 3
    for(i <- new_n to new_n + 2; j <- new_m to new_m + 2 if data(i)(j).info == cellValue && !(i == cursor_x && j == cursor_y))
        data(i)(j).info = -1

    refreshTableState()
    refreshCursorState()
  }

  // dir == 0 - up, 1 - down, 2 - left, 3 - right
  def move(dir : Int) : Unit = {
    restorePreviousCellState()
    dir match {
      case 0 | 'u' =>
        cursor_x = (cursor_x + DIMENSION - 1) % DIMENSION
      case 1 | 'd' =>
        cursor_x = (cursor_x + 1) % DIMENSION
      case 2 | 'l' =>
        cursor_y = (cursor_y + DIMENSION - 1) % DIMENSION
      case 3 | 'r' =>
        cursor_y = (cursor_y + 1) % DIMENSION
    }
    refreshCursorState()
  }

  def executeOperation(file : File) : Unit = {
    val moves : String = FileUtils.getFileContent(file)
    for(singleMove <- moves) {
      if (!singleMove.isDigit)
        move(singleMove)
      else {
        if(!data(cursor_x)(cursor_y).original) {
          data(cursor_x)(cursor_y).info = singleMove.toInt - '0'.toInt
          activeCells(cursor_x)(cursor_y).setText(singleMove.toString)
        }
      }
    }

    refreshCursorState()
  }

  def saveTableIntoFile() : Unit = {
    val tableContent : String = convertCellDataToString()
    FileUtils.createNewFile(tableContent, 0)

    refreshCursorState()
  }

  def saveOperationIntoFile(operations : String) : Unit = {
    FileUtils.createNewFile(operations, 1)

    refreshCursorState()
  }

  def saveSolutionIntoFile() : Unit = {
    val tableContent : String = convertCellDataToString(2)
    FileUtils.createNewFile(tableContent, 2)

    refreshCursorState()
  }

  // mode == 0 - examples ; mode == 1 - operations ; mode == 2 - solutions
  def convertCellDataToString(mode : Int = 0) : String = {
    val stringBuilder : StringBuilder = new StringBuilder
    if (mode != 2) {
      data(cursor_x)(cursor_y).info = 10 // Special mark for cursor
    }
    for(i <- 0 until DIMENSION) {
      for(j <- 0 until DIMENSION) {
        if(data(i)(j).info == -1)
          stringBuilder.append("-")
        else {
          if (data(i)(j).info == 10)
            stringBuilder.append("P")
          else
            stringBuilder.append(data(i)(j).info)
        }
      }
      stringBuilder.append("\n")
    }
    stringBuilder.toString()
  }

  def possibleDigits(board: Array[Array[DataUnit]], r: Int, c: Int): Seq[Int] = {
    def cells(i: Int) =
      Seq(board(r)(i).info, board(i)(c).info, board(s * (r / s) + i / s)(s * (c / s) + i % s).info)
    val used = board.indices.flatMap(cells)
    (1 to 9).diff(used)
  }

  def solve(board: Array[Array[DataUnit]], cell: Int = 0): Option[Array[Array[DataUnit]]] =
    (cell % DIMENSION, cell / DIMENSION) match {
      case (0, 9) => Some(board)
      case (r, c) if board(r)(c).info > 0 => solve(board, cell + 1)
      case (r, c) => possibleDigits(board, r, c)
        .flatMap(n => solve(board.updated(r, board(r).updated(c, new DataUnit(n, board(r)(c).original)))))
        .headOption
  }

  def solveSudoku() : Unit = {
    data = solve(data).get
    saveSolutionIntoFile()
  }

  def isSudokuValid(board: Array[Array[DataUnit]]): Boolean = {
    if (board == null || board.length == 0)
      return true
    val rowLength = board.length
    val columnLength = board(0).length
    for (i <- 0 until rowLength) {
      val rowSet = new mutable.HashSet[Int]()
      val colSet = new mutable.HashSet[Int]()
      val gridSet = new mutable.HashSet[Int]()
      for (j <- 0 until columnLength) {
        if (board(i)(j).info != -1 && !rowSet.add(board(i)(j).info))
          return false
        if (board(j)(i).info != -1 && !colSet.add(board(j)(i).info))
          return false
        val RowIndex = 3 * (i / 3)
        val ColIndex = 3 * (i % 3)
        if (board(RowIndex + j / 3)(ColIndex + j % 3).info != -1 && !gridSet.add(board(RowIndex + j / 3)(ColIndex + j % 3).info))
          return false
      }
    }
    true
  }

  def canBeSolved: Boolean = {
    refreshCursorState()
    isSudokuValid(data)
  }

  def checkIsAllFilled() : Boolean = {
    refreshCursorState()
    for(i <- 0 until DIMENSION; j <- 0 until DIMENSION if data(i)(j).info == -1)
      return false
    true
  }

  // Constructor's body
  initializeData()
}
