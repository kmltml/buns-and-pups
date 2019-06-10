package buns

import scala.swing._
import java.{util => ju}

object Main extends SwingApplication {

  val grid = new Grid(300, torus = true)
  grid.randomize()

  val runTimer = new ju.Timer()
  def newRunTask = new ju.TimerTask {
    override def run() = step()
  }
  var runTask: Option[ju.TimerTask] = None

  var runSpeed: Int = 8

  def startRunTask(): Unit = {
    val t = newRunTask
    runTimer.scheduleAtFixedRate(t, 0, 1000 / runSpeed)
    runTask = Some(t)
  }

  val gridView = new GridView(grid)

  def step(): Unit = {
    grid.step()
    gridView.repaint()
  }

  val rootFrame = new MainFrame {
    title = "Buns & Pups"

    val controlPanel = new BoxPanel(Orientation.Vertical) {
      val stepControl = new BoxPanel(Orientation.Horizontal) {
        contents += Button("step") {
          step()
        }
        val runPauseButton: Button = Button("run") {
          runTask match {
            case None =>
              runPauseButton.text = "pause"
              startRunTask()
            case Some(t) =>
              runPauseButton.text = "run"
              t.cancel()
              runTask = None
          }
          runPauseButton.repaint()
        }
        runPauseButton.preferredSize = new Dimension(70, runPauseButton.preferredSize.height)
        contents += runPauseButton
        contents += Button("+") {
          if(runSpeed < 128) {
            runSpeed *= 2
            runTask.foreach { t =>
              t.cancel()
              startRunTask()
            }
          }
        }
        contents += Button("-") {
          if(runSpeed > 1) {
            runSpeed /= 2
            runTask.foreach { t =>
              t.cancel()
              startRunTask()
            }
          }
        }
      }
      contents += stepControl
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
