package buns

import scala.swing._
import java.awt.Color

class GridView(val grid: Grid, var props: Props) extends Component {

  preferredSize = new Dimension(grid.size * 4, grid.size * 4)

  private def cellColor(cell: Cell): Color = cell match {
    case Cell.Empty() => Color.WHITE
    case Cell.Obstacle() => Color.LIGHT_GRAY
    case Cell.Prey(_) => Color.GREEN
    case Cell.Predator(_, _, hunger) =>
      new Color(255 - hunger * 155 / (props.predMaxHunger * 3 / 2), 0, 0)
  }

  override def paintComponent(g: Graphics2D): Unit = {
    val cellSize = (size.width min size.height) / grid.size
    for {
      x <- 0 until grid.size
      y <- 0 until grid.size
    } {
      g.setColor(cellColor(grid(x, y)))
      g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
    }
  }

}
