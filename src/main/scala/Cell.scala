package buns

import Cell._
import scala.util.Random

class Cell(val bits: Bits) extends AnyVal {

  def state: State = State.forCode(bits & State.Mask)

  def move: Dir = Dir.forCode((bits & MoveMask) >> MoveShift)

  def spawn: Dir = Dir.forCode((bits & SpawnMask) >> SpawnShift)

  def hunger: Int = bits >>> HungerShift

  def isEmpty: Boolean = this match {
    case Empty() => true
    case _ => false
  }

  def step(props: Props, rnd: Random, neighbourhood: Dir => Cell): Cell = this match {
    case Empty() => Empty()
    case Obstacle() => Obstacle()
    case Prey(_) =>
      val neighbours = Dir.values.map(neighbourhood)
      if (neighbours.exists {
        case Predator(_, _, hunger) if hunger >= props.predEatMinHunger => true
        case _ => false
      }) Empty()
      else {
        val empty = Dir.values.filter(neighbourhood(_).isEmpty)
        if (empty.nonEmpty) {
          val spawn =
            if(rnd.nextDouble() <= props.preySpawnChance)
              empty(rnd.nextInt(empty.size))
            else Dir.None
          val moveCandidates = empty diff Seq(spawn)
          val move =
            if(moveCandidates.nonEmpty)
              moveCandidates(rnd.nextInt(moveCandidates.size))
            else Dir.None
          Prey(move, spawn)
        } else {
          Prey(Dir.None, Dir.None)
        }
      }
    case Predator(_, _, hunger) if hunger > props.predMaxHunger => Cell.Empty()
    case Predator(_, _, hunger) =>
      val neighbours = Dir.values.map(neighbourhood)
      val empty = Dir.values.filter(neighbourhood(_).isEmpty)
      val spawn =
        if(empty.nonEmpty && hunger <= props.predSpawnMaxHunger && rnd.nextDouble() <= props.predSpawnChance)
          empty(rnd.nextInt(empty.size))
        else Dir.None
      if (hunger >= props.predEatMinHunger && neighbours.exists {
        case Prey(_, _) => true
        case _ => false
      }) {
        Predator(Dir.None, spawn, 0)
      } else {
        val moveCandidates = empty diff Seq(spawn)
        val move =
          if(moveCandidates.nonEmpty) moveCandidates(rnd.nextInt(moveCandidates.size))
          else Dir.None
        Predator(move, spawn, hunger + 1)
      }
  }

}

object Cell {

  type Bits = Int

  def fromBits(bits: Bits): Cell = new Cell(bits)

  def apply(state: State, move: Dir, spawn: Dir, hunger: Int): Cell =
    new Cell(state.code | move.code << MoveShift | spawn.code << SpawnShift | hunger << HungerShift)

  final val MoveShift = 2
  final val MoveMask = 0xf << MoveShift
  final val SpawnShift = 6
  final val SpawnMask = 0xf << SpawnShift

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

  final val HungerShift = 10


  object Predator {

    def apply(move: Dir, spawn: Dir, hunger: Int): Cell =
      Cell(State.Predator, move, spawn, hunger)

    def unapply(cell: Cell): Option[(Dir, Dir, Int)] = cell.state match {
      case State.Predator => Some((cell.move, cell.spawn, cell.hunger))
      case _ => None
    }

  }

  object Prey {

    def apply(move: Dir, spawn: Dir): Cell =
      Cell(State.Prey, move, spawn, 0)

    def unapply(cell: Cell): Option[(Dir, Dir)] = cell.state match {
      case State.Prey => Some(cell.move, cell.spawn)
      case _ => None
    }

  }

  object Empty {

    def apply(): Cell =
      Cell(State.Empty, Dir.None, Dir.None, 0)

    def unapply(cell: Cell): Boolean =
      cell.state == State.Empty

  }

  object Obstacle {

    def apply(): Cell =
      Cell(State.Obstacle, Dir.None, Dir.None, 0)

    def unapply(cell: Cell): Boolean =
      cell.state == State.Obstacle

  }

}
