package buns

import scala.swing._, scala.swing.event._
import java.{util => ju}
import scala.swing.event.EditDone
import scala.util.Try
import scala.swing.event.ValueChanged

object Main extends SwingApplication {

  val grid = new Grid(100, torus = true)

  var props = Props(1.0, 0, 1.0, 5, 0)

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

  val gridView = new GridView(grid, props)
  val graphView = new GraphView()

  def step(): Unit = {
    grid.step(props)
    graphView += grid.population
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

      val randomisation = new GridBagPanel {
        val emptyWeight = new Slider {
          min = 0
          max = 100
          value = 100
        }
        val obstacleWeight = new Slider {
          min = 0
          max = 100
          value = 0
        }
        val predatorWeight = new Slider {
          min = 0
          max = 100
          value = 10
        }
        val preyWeight = new Slider {
          min = 0
          max = 100
          value = 50
        }
        val randomiseButton = Button("Randomise") {
          grid.randomize(emptyWeight.value, obstacleWeight.value, predatorWeight.value, preyWeight.value)
          gridView.repaint()
        }

        layout(new Label("Randomise weights:")) = new Constraints {
          gridx = 0
          gridy = 0
          gridwidth = 2
        }
        layout(new Label("Empty")) = (0, 1)
        layout(emptyWeight) = (1, 1)
        layout(new Label("Obstacle")) = (0, 2)
        layout(obstacleWeight) = (1, 2)
        layout(new Label("Predator")) = (0, 3)
        layout(predatorWeight) = (1, 3)
        layout(new Label("Prey")) = (0, 4)
        layout(preyWeight) = (1, 4)
        layout(randomiseButton) = new Constraints {
          gridx = 0
          gridy = 5
          gridwidth = 2
        }
      }

      val gridControls = new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Grid size:")
        contents += new TextField(grid.size.toString, 4) {
          maximumSize = new Dimension(maximumSize.width, preferredSize.height)
          reactions += {
            case ValueChanged(_) =>
              Try(text.toInt)
                .filter(s => s > 0 && s <= 1000)
                .foreach { s =>
                  grid.size = s
                  gridView.repaint()
                }
            case e => println(e)
          }
        }
        contents += new CheckBox("toroidal") {
          selected = grid.torus
          reactions += {
            case ButtonClicked(_) =>
              grid.torus = selected
          }
        }
      }

      val propsControls = new GridBagPanel {

        val predSpawnChance = new Slider {
          min = 0
          max = 100
          value = (props.predSpawnChance * max).toInt
          tooltip = "Chance for predator to reproduce if other conditions are met"
        }
        val predSpawnMaxHunger = new Slider {
          min = 0
          max = 30
          value = props.predSpawnMaxHunger
          tooltip = "Predators cannot reproduce if their hunger is above this value"
        }
        val preySpawnChance = new Slider {
          min = 0
          max = 100
          value = (props.preySpawnChance * max).toInt
          tooltip = "Chance for prey to reproduce if other conditions are met"
        }
        val predMaxHunger = new Slider {
          min = 0
          max = 30
          value = props.predMaxHunger
          tooltip = "Predators die when their hunger reaches this value"
        }
        val predEatMinHunger = new Slider {
          min = 0
          max = 30
          value = props.predEatMinHunger
          tooltip = "Predators ignore prey when their hunger is below this value"
        }

        layout(new Label("Predator birth")) = (0, 0)
        layout(predSpawnChance) = (1, 0)
        layout(new Label("Predator breed")) = (0, 1)
        layout(predSpawnMaxHunger) = (1, 1)
        layout(new Label("Prey birth")) = (0, 2)
        layout(preySpawnChance) = (1, 2)
        layout(new Label("Predator starve")) = (0, 3)
        layout(predMaxHunger) = (1, 3)
        layout(new Label("Predator eat")) = (0, 4)
        layout(predEatMinHunger) = (1, 4)

        listenTo(predSpawnChance, predSpawnMaxHunger, preySpawnChance, predMaxHunger, predEatMinHunger)

        reactions += {
          case ValueChanged(_) =>
            props = Props(
              predSpawnChance.value.toDouble / predSpawnChance.max,
              predSpawnMaxHunger.value,
              preySpawnChance.value.toDouble / preySpawnChance.max,
              predMaxHunger.value,
              predEatMinHunger.value
            )
            gridView.props = props
        }
      }

      contents += stepControl
      contents += randomisation
      contents += gridControls
      contents += propsControls
    }

    val splitPanel = new SplitPane(Orientation.Horizontal, gridView, graphView)

    contents = new BorderPanel {
      import BorderPanel.Position._
      layout(splitPanel) = Center
      layout(controlPanel) = East
    }

    pack()
  }

  def startup(args: Array[String]) = {
    rootFrame.open()
  }

}
