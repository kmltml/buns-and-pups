package buns

import scala.swing._

object Main extends SwingApplication {

  val grid = new Grid(20)
  grid.randomize()

  val rootFrame = new MainFrame {
    title = "Buns & Pups"

    val gridView = new GridView(grid)

    val controlPanel = new BoxPanel(Orientation.Vertical) {
      contents += Button("step") {
        grid.step()
        gridView.repaint()
      }
    }

    contents = new BorderPanel {
      import BorderPanel.Position._
      layout(gridView) = Center
      layout(controlPanel) = East
    }

    pack()
  }

  def startup(args: Array[String]) = {
    rootFrame.open()
  }

}
