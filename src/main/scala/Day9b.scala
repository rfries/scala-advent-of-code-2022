import scala.io.Source
import scala.collection.immutable.Queue

object Day9b:

  def main(args: Array[String]): Unit =
    //val lines = Source.fromString(input).getLines()
    //val lines = Source.fromString(input2).getLines()
    val lines = Source.fromFile("day9.in.txt").getLines()

    val moves: Queue[Move] = linesToMoves(lines)
    println(moves)
    val positions: Queue[Vector[Pos]] = movesToPositions(moves)
    val allTailPos = positions.map(_.last).sortBy(p => (p.x, p.y)).distinct
    println(s"Tails visited ${allTailPos.length} distinct positions.")

  enum Move(val cmd: String, val delta: Pos):
    case Up     extends Move("U", Pos(1, 0))
    case Down   extends Move("D", Pos(-1, 0))
    case Left   extends Move("L", Pos(0, -1))
    case Right  extends Move("R", Pos(0, 1))
  
  val numKnots = 9

  case class Pos(x: Int, y: Int):
    def add(other: Pos): Pos = Pos(x + other.x, y + other.y)

    def follow(other: Pos): Pos =
      import Math.* 
      if abs(x - other.x) > 1 || abs(y - other.y) > 1 then
        Pos(
          x + other.x.compare(x).sign,
          y + other.y.compare(y).sign
        )
      else
        this
        
  def linesToMoves(lines: Iterator[String]): Queue[Move] =
    lines.foldLeft(Queue.empty[Move]) { (moves, line) =>
      line match
        case s"$cmd $num" =>
          val move = Move.values.find(_.cmd == cmd).getOrElse(throw new IllegalArgumentException(s"Unknown command: $cmd"))
          moves.enqueueAll(Queue.fill(num.toInt)(move))
        case _ => throw new IllegalArgumentException(s"Bad input: $line")
    }
  
  def movesToPositions(moves: Queue[Move]): Queue[Vector[Pos]] =
    moves.foldLeft(Queue(Vector.fill(numKnots + 1)(Pos(0,0)))) { (q, move) =>
      q.lastOption match
        case Some(posVec) =>

          posVec match
            case hpos +: tails =>
              val newHead = hpos.add(move.delta)
              val newVec = tails.scanLeft(newHead)((leader, us) => us.follow(leader))
              q.appended(newVec)
              
            case _ => throw new IllegalArgumentException(s"Pos vector too small.")

        case None => throw new IllegalStateException(s"No initial position.")
    }

  val input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""
  val input2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""
