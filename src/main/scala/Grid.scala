package buns
import scala.util.Random

class Grid(val size: Int, val torus: Boolean) {

  private var currentCells: Array[Cell.Bits] =
    Array.fill(size * size)(Cell.Empty().bits)

  private var nextCells: Array[Cell.Bits] =
    Array.fill(size * size)(Cell.Empty().bits)

  def randomize(emptyw: Double, obstaclew: Double, predatorw: Double, preyw: Double): Unit = {
    val weights = Seq(emptyw, obstaclew, predatorw, preyw)
    val weightSum = weights.sum
    val accumWeights = weights.scan(0.0)(_ + _).tail.map(_ / weightSum)
    currentCells = Array.fill(size * size) {
      val r = util.Random.nextDouble()
      (accumWeights.indexWhere(r < _) match {
        case 0 => Cell.Empty()
        case 1 => Cell.Obstacle()
        case 2 => Cell.Predator(Dir.None, Dir.None, 0)
        case 3 => Cell.Prey(Dir.None, Dir.None)
      }).bits
    }
  }

  private def wrap(x: Int): Int =
    if(x < 0) wrap(x + size)
    else if(x >= size) wrap(x - size)
    else x

  def apply(x: Int, y: Int): Cell =
    if(torus) {
      Cell.fromBits(currentCells(wrap(y) * size + wrap(x)))
    } else {
      if (x < 0 || x >= size || y < 0 || y >= size)
        Cell.Obstacle()
      else
        Cell.fromBits(currentCells(y * size + x))
    }

  def update(x: Int, y: Int, c: Cell): Unit = {
    if (torus) {
      nextCells(wrap(y) * size + wrap(x)) = c.bits
    } else {
      if (x >= 0 && x < size && y >= 0 && y < size) {
        nextCells(y * size + x) = c.bits
      }
    }
  }

  def neighbourhood(x: Int, y: Int)(dir: Dir): Cell =
    apply(x + dir.dx, y + dir.dy)

  def step(): Unit = {
    val rand = new Random
    for {
      x <- (0 until size).par
      y <- 0 until size
    } {
      this(x, y) = this(x, y).step(rand, neighbourhood(x, y)).bits
    }

    for {
      x <- 0 until size
      y <- 0 until size
      if this(x, y).isEmpty
    } {
      val n = neighbourhood(x, y) _
      val neighbours = Dir.values.map(d => (d, n(d)))
      val spawn = neighbours.collectFirst {
        case (d, Cell.Predator(_, s, _)) if s == -d =>
          Cell.Predator(Dir.None, Dir.None, 1)
        case (d, Cell.Prey(_, s)) if s == -d =>
          Cell.Prey(Dir.None, Dir.None)
      }
      spawn match {
        case Some(c) => this(x, y) = c
        case None =>
          val moves = neighbours.collect {
            case (d, Cell.Predator(m, _, h)) if m == -d =>
              (d, Cell.Predator(Dir.None, Dir.None, h))
            case (d, Cell.Prey(m, _)) if m == -d =>
              (d, Cell.Prey(Dir.None, Dir.None))
          }
          moves match {
            case Seq((d, c)) =>
              this(x + d.dx, y + d.dy) = Cell.Empty()
              this(x, y) = c
            case _ =>
          }
      }
    }

    val tmp = nextCells
    nextCells = currentCells
    currentCells = nextCells
  }

}
