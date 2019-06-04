package buns

import Cell._

class Cell(private val bits: Int) extends AnyVal {

  def state: State = State.forCode(bits & State.Mask)

  def dir: Dir = Dir.forCode(bits & Dir.Mask)

  def hunger: Int = bits >>> HungerShift

}

object Cell {

  def apply(state: State, direction: Dir, hunger: Int): Cell =
    new Cell(state.code | direction.code | hunger << HungerShift)

  class State private (val code: Int) extends AnyVal

  object State {

    final val Mask = 0x3
    final val Empty = new State(0)
    final val Predator = new State(1)
    final val Prey = new State(2)
    final val Obstacle = new State(3)

    def forCode(code: Int): State =
      new State(code & Mask)

  }

  sealed abstract class Dir(val code: Int, val dx: Int, val dy: Int)

  object Dir {
    final val Mask = 0x7 << 2
    case object N extends Dir(0, 0, -1)
    case object NE extends Dir(1, 1, -1)
    case object E extends Dir(2, 1, 0)
    case object SE extends Dir(3, 1, 1)
    case object S extends Dir(4, 0, 1)
    case object SW extends Dir(5, -1, 1)
    case object W extends Dir(6, -1, 0)
    case object NW extends Dir(7, -1, -1)

    val forCode: Map[Int, Dir] =
      Seq(N, NE, E, SE, S, SW, W, NW).map(d => d.code -> d).toMap

  }

  final val HungerShift = 5


  object Predator {

    def apply(dir: Dir, hunger: Int): Cell =
      Cell(State.Predator, dir, hunger)

    def unapply(cell: Cell): Option[(Dir, Int)] = cell.state match {
      case State.Predator => Some((cell.dir, cell.hunger))
      case _ => None
    }

  }

  object Prey {

    def apply(dir: Dir): Cell =
      Cell(State.Prey, dir, 0)

    def unapply(cell: Cell): Option[Dir] = cell.state match {
      case State.Prey => Some(cell.dir)
      case _ => None
    }

  }

  object Empty {

    def apply(): Cell =
      Cell(State.Empty, Dir.N, 0)

    def unapply(cell: Cell): Boolean =
      cell.state == State.Empty

  }

  object Obstacle {

    def apply(): Cell =
      Cell(State.Obstacle, Dir.N, 0)

    def unapply(cell: Cell): Boolean =
      cell.state == State.Obstacle

  }

}
