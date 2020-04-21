package gui

import java.awt._
import java.awt.event._

object GUI {
  val HEIGHT = 640
  val WIDTH = 640

  class MainFrame extends Frame("SudokuGame") {
    private val cardLayout : CardLayout = new CardLayout

    private def addCards(): Unit = {
      val mainCard : MainCard = new MainCard(cardLayout, this)
      add("MainCard", mainCard)

      val newGameCard : NewGameCard = new NewGameCard(cardLayout, this)
      add("NewGameCard", newGameCard)

      val createNewGameCard : CreateNewGameCard = new CreateNewGameCard(cardLayout, this)
      add("CreateNewGameCard", createNewGameCard)

      val playGameCard : PlayGameCard = new PlayGameCard(cardLayout, this)
      add("PlayGameCard", playGameCard)

      val editTableCard : EditTableCard = new EditTableCard(cardLayout, this)
      add("EditTableCard", editTableCard)
    }

    // Constructor's body
    setLayout(cardLayout)
    addCards()
    setSize(WIDTH, HEIGHT)
    addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        dispose()
      }
    })
    setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    new MainFrame
  }
}