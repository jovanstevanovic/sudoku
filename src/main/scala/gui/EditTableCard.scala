package gui

import java.awt._
import java.awt.event._
import java.io.File

import javax.swing._
import javax.swing.border.EmptyBorder
import javax.swing.event.{DocumentEvent, DocumentListener}
import model.{FileUtils, Model}

class EditTableCard(val cardLayout: CardLayout, val parentsContainer : Container) extends JPanel {
  // Constants
  val TITLE_PADDING = 15

  // Class fields
  val namedOperationsTextField : JTextField = new JTextField()
  val gameStatusLabel : JLabel = new JLabel("Status: Not checked!")
  val cells : Array[Array[JTextField]] = Array.ofDim[JTextField](Model.DIMENSION, Model.DIMENSION)

  private def createMatrix() : JPanel = {
    val matrixPanel : JPanel = new JPanel
    val groupLayout : GroupLayout = new GroupLayout(matrixPanel)
    val cellFont : Font = new Font("SansSerif", Font.ITALIC, 14)

    matrixPanel.setLayout(groupLayout)
    groupLayout.setAutoCreateGaps(true)
    groupLayout.setAutoCreateContainerGaps(true)

    for(i <- 0 until Model.DIMENSION; j <- 0 until Model.DIMENSION) {
      cells(i)(j) = new JTextField()
      cells(i)(j).setFont(cellFont)
      cells(i)(j).getDocument.addDocumentListener(new DocumentListener {
        override def insertUpdate(e: DocumentEvent): Unit = {
          if (e.getOffset == 0) // Added first number
            Model.updateDate(i, j, cells(i)(j).getText.toInt)
        }

        override def removeUpdate(e: DocumentEvent): Unit = {
          if (e.getOffset == 0) // Deleted last number
            Model.updateDate(i, j, -1)
        }

        override def changedUpdate(e: DocumentEvent): Unit = {}
      })
    }

    Model.initCellsInModel(cells, 1)

    val sequentialGroupHorizontal = groupLayout.createSequentialGroup()
    for(i <- 0 until Model.DIMENSION) {
      val group = groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
      for(j <- 0 until Model.DIMENSION)
        group.addComponent(cells(j)(i))
      sequentialGroupHorizontal.addGroup(group)
    }
    groupLayout.setHorizontalGroup(sequentialGroupHorizontal)

    val sequentialGroupVertical = groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
    for(i <- 0 until Model.DIMENSION) {
      val group = groupLayout.createSequentialGroup()
      for(j <- 0 until Model.DIMENSION)
        group.addComponent(cells(j)(i))
      sequentialGroupVertical.addGroup(group)
    }
    groupLayout.setVerticalGroup(sequentialGroupVertical)

    matrixPanel
  }

  private def disableAllInputs() : Unit = {
    for(i <- 0 until Model.DIMENSION; j <- 0 until Model.DIMENSION) {
      cells(i)(j).setFocusable(false)
    }
  }

  private def fillCard() : Unit = {
    // Fonts
    val titleLabelFont : Font = new Font("SansSerif", Font.BOLD + Font.ITALIC, 24)
    val genericFont : Font = new Font("SansSerif", Font.ITALIC, 18)
    val buttonFont : Font = new Font("SansSerif", Font.ITALIC, 14)

    // Main panel
    val mainPanel : Panel = new Panel
    val groupLayout : GroupLayout = new GroupLayout(mainPanel)
    mainPanel.setLayout(groupLayout)
    groupLayout.setAutoCreateGaps(true)
    groupLayout.setAutoCreateContainerGaps(true)

    // Game Title label
    val gameTitleLabel : JLabel = new JLabel("Sudoku Game")
    gameTitleLabel.setFont(titleLabelFont)
    gameTitleLabel.setBorder(new EmptyBorder(0, 0, TITLE_PADDING, 0))

    // Matrix of cells
    val matrixPanel = createMatrix()

    // Game status label
    gameStatusLabel.setFont(genericFont)

    // Direction buttons
    val upButton: JButton = new JButton("Up")
    val downButton: JButton = new JButton("Down")
    val leftButton: JButton = new JButton("Left")
    val rightButton: JButton = new JButton("Right")

    upButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.move(0)
      }
    })

    downButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.move(1)
      }
    })

    leftButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.move(2)
      }
    })

    rightButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.move(3)
      }
    })

    // Transpose Matrix Button
    val transposeMatrixButton : JButton = new JButton("Transpose")
    transposeMatrixButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.transposeMatrix()
      }
    })
    transposeMatrixButton.setFont(buttonFont)

    // Complement Matrix Button
    val complementMatrixButton : JButton = new JButton("Complement")
    complementMatrixButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.complementMatrix()
      }
    })
    complementMatrixButton.setFont(buttonFont)

    // Filter Row/Column Button
    val filterRowColumnButton : JButton = new JButton("Filter R/C")
    filterRowColumnButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.filterRowAndColumn()
      }
    })
    filterRowColumnButton.setFont(buttonFont)

    // Filter SubMatrix
    val filterSubMatrixButton : JButton = new JButton("Filter SubMatrix")
    filterSubMatrixButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        Model.filterSubMatrix()
      }
    })
    filterSubMatrixButton.setFont(buttonFont)

    // Named Operations Label
    val namedOperationsLabel : JLabel = new JLabel("Operation")
    namedOperationsLabel.setFont(genericFont)

    // Named Operations Text Field
    namedOperationsTextField.setFont(genericFont)

    // Named Operations File Creating
    val saveOperationFileButton: JButton = new JButton("Save Operation")
    saveOperationFileButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (!namedOperationsTextField.getText().isEmpty) {
          Model.saveOperationIntoFile(namedOperationsTextField.getText)
          gameStatusLabel.setText("Status: Operation saved in file_" + FileUtils.nextIdOperations + ".txt!")
          namedOperationsTextField.setText("")
        } else {
          gameStatusLabel.setText("Status: Operation is empty!")
        }
      }
    })
    saveOperationFileButton.setFont(buttonFont)

    // Check Solution Button
    val checkSolutionButton : JButton = new JButton("Check Solution")
    checkSolutionButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (Model.canBeSolved) {
          gameStatusLabel.setText("Status: Can be solved! :)")
        } else {
          gameStatusLabel.setText("Status: Can't be solved! :( Check numbers!")
        }
      }
    })
    checkSolutionButton.setFont(buttonFont)

    // Solve Sudoku Button
    val saveNewSudokuTableButton : JButton = new JButton("Save Table")
    saveNewSudokuTableButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (Model.canBeSolved) {
          Model.saveTableIntoFile()
          gameStatusLabel.setText("Status: Sudoku table saved in file_" + FileUtils.nextIdExamples + ".txt!")
        } else {
          gameStatusLabel.setText("Status: Can't be saved! :( Solution is incorrect!")
        }
      }
    })
    saveNewSudokuTableButton.setFont(buttonFont)

    // Exit Editing Button
    val exitEditingButton : JButton = new JButton("Exit editing")
    exitEditingButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        gameStatusLabel.setText("Status: Not checked!")
        Model.restorePreviousCellState()
        cardLayout.show(parentsContainer, "MainCard")
      }
    })
    exitEditingButton.setFont(buttonFont)

    groupLayout.setHorizontalGroup(
      groupLayout.createSequentialGroup()
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(gameTitleLabel)
          .addComponent(matrixPanel)
          .addComponent(gameStatusLabel)
          .addGroup(groupLayout.createSequentialGroup()
            .addComponent(upButton)
            .addComponent(downButton)
            .addComponent(leftButton)
            .addComponent(rightButton))
          .addGroup(groupLayout.createSequentialGroup()
            .addComponent(transposeMatrixButton)
            .addComponent(complementMatrixButton)
            .addComponent(filterRowColumnButton)
            .addComponent(filterSubMatrixButton))
          .addGroup(groupLayout.createSequentialGroup()
            .addComponent(namedOperationsLabel)
            .addComponent(namedOperationsTextField)
            .addComponent(saveOperationFileButton))
          .addGroup(groupLayout.createSequentialGroup()
            .addComponent(checkSolutionButton)
            .addComponent(saveNewSudokuTableButton))
          .addComponent(exitEditingButton))
    );

    groupLayout.setVerticalGroup(
      groupLayout.createSequentialGroup()
        .addComponent(gameTitleLabel)
        .addComponent(matrixPanel)
        .addComponent(gameStatusLabel)
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(upButton)
          .addComponent(downButton)
          .addComponent(leftButton)
          .addComponent(rightButton))
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(transposeMatrixButton)
          .addComponent(complementMatrixButton)
          .addComponent(filterRowColumnButton)
          .addComponent(filterSubMatrixButton))
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(namedOperationsLabel)
          .addComponent(namedOperationsTextField)
          .addComponent(saveOperationFileButton))
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(checkSolutionButton)
          .addComponent(saveNewSudokuTableButton))
        .addComponent(exitEditingButton)
    );

    setLayout(new GridBagLayout)
    add(mainPanel)
  }

  // Constructor's body
  fillCard()
}
