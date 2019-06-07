package buns

import Dir._

sealed abstract class Dir(val code: Int, val dx: Int, val dy: Int) {

  def unary_- : Dir = this match {
    case None => None
    case N => S
    case NE => SW
    case E => W
    case SE => NW
    case S => N
    case SW => NE
    case W => E
    case NW => SE
  }

}

object Dir {
  case object None extends Dir(0, 0, 0)
  case object N extends Dir(1, 0, -1)
  case object NE extends Dir(2, 1, -1)
  case object E extends Dir(3, 1, 0)
  case object SE extends Dir(4, 1, 1)
  case object S extends Dir(5, 0, 1)
  case object SW extends Dir(6, -1, 1)
  case object W extends Dir(7, -1, 0)
  case object NW extends Dir(8, -1, -1)

  val values = Seq(N, NE, E, SE, S, SW, W, NW)

  val forCode: Map[Int, Dir] =
    (None +: values).map(d => d.code -> d).toMap

}
