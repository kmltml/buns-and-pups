package buns

import scala.swing._

object Main extends SwingApplication {

  val rootFrame = new MainFrame {
    title = "Buns & Pups"

    contents = new BorderPanel {
      import BorderPanel.Position._
      layout(new GridView) = Center
    }

    pack()
  }

  def startup(args: Array[String]) = {
    rootFrame.open()
  }

}
