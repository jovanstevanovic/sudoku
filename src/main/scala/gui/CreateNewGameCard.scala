package gui

import java.awt._
import java.awt.event._
import java.io.File

import javax.swing._
import javax.swing.border.EmptyBorder
import model.{FileUtils, Model}

class CreateNewGameCard(val cardLayout: CardLayout, val parentsContainer : Container) extends JPanel {
  // Constants
  val TITLE_PADDING = 15
  val SUBTITLE_PADDING = 50

  // Class fields
  var defaultTargetFileName : String = ""
  val fileChooser : JFileChooser = new JFileChooser(FileUtils.PATH_TO_EXAMPLES)

  private def createButtonGroupPanel(): JPanel = {
    val examplesFileNameArray : Array[String] = FileUtils.readAllFilesFromDirectory(0)
    val buttonGroupPanel : JPanel = new JPanel
    val buttonGroup : ButtonGroup = new ButtonGroup
    var firstIteration : Boolean = true

    buttonGroupPanel.setLayout(new GridLayout(examplesFileNameArray.length, 1))
    for (fileName <- examplesFileNameArray) {
      val radioButton : JRadioButton = new JRadioButton(fileName)
      if (firstIteration) {
        firstIteration = false
        defaultTargetFileName = examplesFileNameArray(0)
        radioButton.setSelected(true)
      }
      radioButton.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          defaultTargetFileName = e.getActionCommand
        }
      })
      buttonGroup.add(radioButton)
      buttonGroupPanel.add(radioButton)
    }
    buttonGroupPanel
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

    // Subtitle label
    val subTitleLabel : JLabel = new JLabel("~ Create New Game ~")
    subTitleLabel.setFont(genericFont)
    subTitleLabel.setBorder(new EmptyBorder(0, 0, SUBTITLE_PADDING, 0))

    // Load File Label
    val loadFileLabel : JLabel = new JLabel("Load file:")
    loadFileLabel.setFont(genericFont)

    // Load File Button
    val loadFileButton: JButton = new JButton("Load File")
    var selectedFile : File = new File(".")
    loadFileButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val result = fileChooser.showOpenDialog(CreateNewGameCard.this)
        if (result == JFileChooser.APPROVE_OPTION) {
          selectedFile = fileChooser.getSelectedFile
          changeCardTo(selectedFile)
        }

      }
    })
    loadFileButton.setFont(buttonFont)

    groupLayout.setHorizontalGroup(
      groupLayout.createSequentialGroup()
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(gameTitleLabel)
          .addComponent(subTitleLabel)
          .addGroup(groupLayout.createSequentialGroup()
            .addComponent(loadFileLabel)
            .addComponent(loadFileButton)))
    );

    groupLayout.setVerticalGroup(
      groupLayout.createSequentialGroup()
        .addComponent(gameTitleLabel)
        .addComponent(subTitleLabel)
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(loadFileLabel)
          .addComponent(loadFileButton))
    );

    setLayout(new GridBagLayout)
    add(mainPanel)
  }

  private def changeCardTo(file: File): Unit = {
    if (FileUtils.isFileAppropriate(file.getName)) {
      Model.loadFile(file, 1) // mode == 0 - play game mode ; mode == 1 - edit sudoku table
      cardLayout.show(parentsContainer, "EditTableCard")
    }
  }

  // Constructor's body
  fillCard()
}
