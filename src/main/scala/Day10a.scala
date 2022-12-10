import scala.io.Source
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Day10a:


  def main(args: Array[String]): Unit =
    //val lines = Source.fromString(input).getLines()
    val lines = Source.fromFile("day10.in.txt").getLines()

    val hist = generateHist(lines)

    val strengths = (20 to 220 by 40).map(n => (n, hist(n-1)*n))
    
    strengths.foreach { (n, strength) =>
      println(s"The signal strength for the ${n}th cycle is $strength.")
    }
    println(s"The sum of the signal strengths is ${strengths.map(_._2).sum}.")


  def generateHist(lines: Iterator[String]): Vector[Int] =
    lines.foldLeft(Vector(1)) { (timeline, line) =>
      val x = timeline.last
      line match
        case s"addx $arg" => timeline :++ Seq(x, x + arg.toInt)
        case s"noop"      => timeline :+ x
        case _            => throw new IllegalArgumentException(s"Bad line: $line")
    }

    // @tailrec
    // def run(lines: Iterator[String], hist: Queue[Int], set: Option[Int]): Queue[Int] =



  val input = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""
