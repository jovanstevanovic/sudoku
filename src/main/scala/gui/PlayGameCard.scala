package gui

import java.awt._
import java.awt.event._
import java.io.File

import javax.swing._
import javax.swing.border.EmptyBorder
import javax.swing.event.{DocumentEvent, DocumentListener}
import model.{FileUtils, Model}

class PlayGameCard(val cardLayout: CardLayout, val parentsContainer : Container) extends JPanel {
  // Constants
  val TITLE_PADDING = 15

  // Class fields
  var defaultTargetFileName : String = ""
  val fileChooser : JFileChooser = new JFileChooser(FileUtils.PATH_TO_OPERATIONS)
  val gameStatusLabel : JLabel = new JLabel("Status: Not solved!")
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
            Model.setCellValue(i, j, cells(i)(j).getText.toInt)
        }

        override def removeUpdate(e: DocumentEvent): Unit = {
          if (e.getOffset == 0) // Deleted last number
            Model.setCellValue(i, j, -1)
        }

        override def changedUpdate(e: DocumentEvent): Unit = {}
      })
    }

    Model.initCellsInModel(cells, 0)

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
      cells(i)(j).setEnabled(false)
    }
  }

  private def enableAllInputs() : Unit = {
    for(i <- 0 until Model.DIMENSION; j <- 0 until Model.DIMENSION) {
      cells(i)(j).setEnabled(true)
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

    // Load Operation File Button
    val loadOperationFileButton: JButton = new JButton("Load Operation")
    var selectedFile : File = new File(".")
    loadOperationFileButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val result = fileChooser.showOpenDialog(PlayGameCard.this)
        if (result == JFileChooser.APPROVE_OPTION) {
          selectedFile = fileChooser.getSelectedFile
          Model.executeOperation(selectedFile)
        }
      }
    })
    loadOperationFileButton.setFont(buttonFont)

    // Check Solution Button
    val checkSolutionButton : JButton = new JButton("Check Solution")
    checkSolutionButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (!Model.checkIsAllFilled()) {
          gameStatusLabel.setText("Status: Not all cells are filled!")
        } else {
          if (Model.canBeSolved) {
            gameStatusLabel.setText("Status: Resolved! Congrats! :)")
            disableAllInputs()
          } else {
            gameStatusLabel.setText("Status: Solution is incorrect! :( Try again!")
          }
        }
      }
    })
    checkSolutionButton.setFont(buttonFont)

    // Solve Sudoku Button
    val solveSudokuButton : JButton = new JButton("Solve Sudoku")
    solveSudokuButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if(Model.canBeSolved) {
          Model.solveSudoku()
          gameStatusLabel.setText("Status: Solution is exported to file: file_" + FileUtils.nextIdSolutions + ".txt!")
          disableAllInputs()
        } else {
          gameStatusLabel.setText("Status: Can't be saved! Please remove wrong numbers!")
        }
      }
    })
    solveSudokuButton.setFont(buttonFont)

    // Exit Game Button
    val exitGameButton : JButton = new JButton("Exit game")
    exitGameButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        enableAllInputs()
        Model.restorePreviousCellState()
        gameStatusLabel.setText("Status: Not solved!")
        cardLayout.show(parentsContainer, "MainCard")
      }
    })
    exitGameButton.setFont(buttonFont)

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
            .addComponent(loadOperationFileButton)
            .addComponent(checkSolutionButton)
            .addComponent(solveSudokuButton))
          .addComponent(exitGameButton))
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
          .addComponent(loadOperationFileButton)
          .addComponent(checkSolutionButton)
          .addComponent(solveSudokuButton))
        .addComponent(exitGameButton)
    );

    setLayout(new GridBagLayout)
    add(mainPanel)
  }

  // Constructor's body
  fillCard()
}
