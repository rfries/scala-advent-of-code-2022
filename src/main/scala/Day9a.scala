import scala.collection.mutable.{ArraySeq => BadMutable}
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.immutable.Queue
import javax.swing.text.Position

object Day9a:

  enum Move(val cmd: String, val delta: Pos):
    case Up     extends Move("U", Pos(1, 0))
    case Down   extends Move("D", Pos(-1, 0))
    case Left   extends Move("L", Pos(0, -1))
    case Right  extends Move("R", Pos(0, 1))

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
  
  def movesToPositions(moves: Queue[Move]): Queue[(Pos, Pos)] =
    moves.foldLeft(Queue((Pos(0,0), Pos(0,0)))) { (q, move) =>
      q.lastOption match
        case Some((h, t)) =>
          val newh = h.add(move.delta)
          val newt = t.follow(newh)
          q.enqueue((newh, newt))

        case None => throw new IllegalStateException(s"No initial position.")
    }

  def main(args: Array[String]): Unit =
    println("Hello Day8b!")
    //val lines = Source.fromString(input).getLines()
    val lines = Source.fromFile("day9.in.txt").getLines()

    val moves = linesToMoves(lines)
    val positions = movesToPositions(moves)
    val tpos = positions.map(_._2).sortBy(p => (p.x, p.y)).distinct

    println(s"Tail visited ${tpos.length} distinct positions.")


  val input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""

