package buns

import scala.swing._
import scala.collection.mutable
import _root_.java.awt.Color
import scala.swing.event.MouseClicked

class GraphView extends Component {

  preferredSize = new Dimension(200, 200)

  private val history: mutable.Queue[Population] = mutable.Queue.empty

  private var stacked: Boolean = false

  listenTo(mouse.clicks)

  reactions += {
    case _: MouseClicked =>
      stacked = !stacked
      repaint()
  }

  def +=(pop: Population): Unit = {
    history += pop
    while (history.size > size.width) {
      history.dequeue()
    }
    repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {

    def scale(y: Double): Int =
      (size.getHeight * (1.0 - y)).toInt

    val predColor = Color.RED
    val preyColor = Color.GREEN
    if(stacked) {
      for ((y, x) <- history.toSeq.zipWithIndex) {
        val predy = scale(y.predator)
        g.setColor(predColor)
        g.drawLine(x, scale(0.0), x, predy)
        g.setColor(preyColor)
        g.drawLine(x, predy, x, scale(y.prey + y.predator))
      }
    } else {
      for((Seq(prev, y), x) <- history.toSeq.sliding(2).zipWithIndex) {
        g.setColor(predColor)
        g.drawLine(x, scale(prev.predator), x + 1, scale(y.predator))
        g.setColor(preyColor)
        g.drawLine(x, scale(prev.prey), x + 1, scale(y.prey))
      }
    }
  }

}
