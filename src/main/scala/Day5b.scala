
import Day5b.*
import scala.collection.immutable.Queue
import scala.io.Source

val in_day5b =
"""
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

@main def day5bmain(): Unit =

  println("Hello Day5!")

  //val lines = Source.fromString(in_day5).getLines
  val lines = Source.fromFile("day5.in.txt").getLines

  lines.foreach {
    case diagRE(line) =>
      if stacks.length == 0 then
        val cols = (line.length + 1) / 4
        println(s"Initializing for $cols stacks")
        stacks = Array.fill(cols)(List.empty)
      updateStackItems(line)

    case moveRE(num, from, to)  => move(num.toInt, from.toInt-1, to.toInt-1)

    case _ =>
  }

  (0 until stacks.length).foreach { n =>
    println(stacks(n))
  }
  val result = (0 until stacks.length).map(stacks(_).head).mkString
  println(s"Result: $result")


object Day5b:

  var stacks: Array[List[Char]] = Array.empty

  val diagRE    = """(\s*\[\w+\].*)""".r
  val moveRE    = """move (\d+) from (\d+) to (\d+)""".r

  def updateStackItems(line: String): Unit =
    val pad = line.padTo(10, ' ')
    (0 until stacks.length).foreach { n =>
      val col = n * 4 + 1
      val item = pad(col)
      if !item.isWhitespace then
        stacks(n) = stacks(n).appended(item)
    }

  def move(num: Int, from: Int, to: Int): Unit =
    val items = stacks(from).take(num)
    stacks(from) = stacks(from).drop(num)
    stacks(to) = items ++ stacks(to)
