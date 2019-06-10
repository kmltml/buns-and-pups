package buns

import scala.swing._
import java.awt.Color

class GridView(private var _grid: Grid) extends Component {

  preferredSize = new Dimension(_grid.size * 2, _grid.size * 2)

  private def cellColor(cell: Cell): Color = cell match {
    case Cell.Empty() => Color.WHITE
    case Cell.Obstacle() => Color.LIGHT_GRAY
    case Cell.Prey(_) => Color.GREEN
    case Cell.Predator(_, _, _) => Color.RED
  }

  override def paintComponent(g: Graphics2D): Unit = {
    val cellSize = (size.width min size.height) / _grid.size
    for {
      x <- 0 until _grid.size
      y <- 0 until _grid.size
    } {
      g.setColor(cellColor(_grid(x, y)))
      g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
    }
  }

}
