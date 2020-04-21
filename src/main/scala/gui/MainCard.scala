package gui

import java.awt._
import java.awt.event._

import javax.swing._
import javax.swing.border.EmptyBorder

class MainCard(val cardLayout: CardLayout, val parentsContainer : Container) extends JPanel {
  // Constants
  val PADDING = 50

  private def fillCard() : Unit = {
    // Fonts
    val fontLabel : Font = new Font("SansSerif", Font.BOLD + Font.ITALIC, 24)
    val fontButton : Font = new Font("SansSerif", Font.ITALIC, 18)

    // Main panel.
    val mainPanel : Panel = new Panel
    val groupLayout : GroupLayout = new GroupLayout(mainPanel)
    mainPanel.setLayout(groupLayout)
    groupLayout.setAutoCreateGaps(true)
    groupLayout.setAutoCreateContainerGaps(true)

    // Game Title label
    val gameTitleLabel : JLabel = new JLabel("Sudoku Game")
    gameTitleLabel.setFont(fontLabel)
    gameTitleLabel.setBorder(new EmptyBorder(0, 0, PADDING, 0))

    // New Game Button
    val newGameButton: JButton = new JButton("New Game")
    newGameButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        changeCardTo("NewGame")
      }
    })
    newGameButton.setFont(fontButton)

    // Create New Game Button
    val createNewGameButton : JButton = new JButton("Create New Game")
    createNewGameButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        changeCardTo("CreateNewGame")
      }
    })
    createNewGameButton.setFont(fontButton)

    // Exit Game Button
    val exitGameButton : JButton = new JButton("Exit Game")
    exitGameButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        changeCardTo("ExitGame")
      }
    })
    exitGameButton.setFont(fontButton)

    groupLayout.setHorizontalGroup(
      groupLayout.createSequentialGroup()
        .addGroup(groupLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
          .addComponent(gameTitleLabel)
          .addComponent(newGameButton)
          .addComponent(createNewGameButton)
          .addComponent(exitGameButton))
    );

    groupLayout.setVerticalGroup(
      groupLayout.createSequentialGroup()
          .addComponent(gameTitleLabel)
          .addComponent(newGameButton)
          .addComponent(createNewGameButton)
          .addComponent(exitGameButton)
    );

    setLayout(new GridBagLayout)
    add(mainPanel)
  }

  private def changeCardTo(s : String): Unit = {
    s match {
      case "NewGame" => cardLayout.show(parentsContainer, "NewGameCard")
      case "CreateNewGame" => cardLayout.show(parentsContainer, "CreateNewGameCard")
      case "ExitGame" => System.exit(0)
    }
  }

  // Constructor's body
  fillCard()
}
