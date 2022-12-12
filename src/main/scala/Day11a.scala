import scala.io.Source
import scala.collection.immutable.Queue
import scala.annotation.newMain

object Day11a:

  def main(args: Array[String]): Unit =
    val lines = Source.fromString(input).getLines().mkString("\n")
    //val lines = Source.fromFile("day11.in.txt").getLines().mkString("\n")

    val startMonkeys = oneMonkeyRegex.findAllMatchIn(lines)
      .map(_.subgroups)
      .map(Monkey.fromList)
      .toVector

    List (20, 1000, 2000, 3000, 4000).foreach {n =>
      println(s"Monkey Business ($n rounds): ${monkeyBusiness(startMonkeys, n)}")
    }
    // val after20 = after(startMonkeys, 20)
    // val sums20 = after20.map(_.itemsProcessed).sorted
    // val monkeyBusiness20 = sums20.sorted.takeRight(2).product
    // println(s"Monkey Business (20 rounds): ${monkeyBusiness(startMonkeys, 20)")

    // val after10000 = after(startMonkeys, 10000)
    // val sums10000 = after10000.map(_.itemsProcessed).sorted
    // println(sums10000)
    // val monkeyBusiness10000 = sums10000.sorted.takeRight(2).product
    // println(s"Monkey Business (10000 rounds): $monkeyBusiness10000")

  end main

  def monkeyBusiness(startMonkeys: Vector[Monkey], rounds: Int): Long =
    val afterMonkeys = after(startMonkeys, rounds).map(_.itemsProcessed).sorted
    println(s"counts: $afterMonkeys")
    afterMonkeys.takeRight(2).product

  def after(startMonkeys: Vector[Monkey], n: Int): Vector[Monkey] =
      (0 until n).foldLeft(startMonkeys) { (monkeys, round) =>
        val itemQs = monkeys.map(_.items)
        val (endMonkeys, endQs) = monkeys.foldLeft((Vector.empty[Monkey]), itemQs) {
          case ((newMonkeys, qs), monkey) =>
            val (newMonkey, updatedQs) = monkey.round(qs)
            (newMonkeys :+ newMonkey, updatedQs)
        }
        endMonkeys.map(m => m.copy(items = endQs(m.num)))
      }

  case class Monkey(num: Int,
    items: Queue[Long],
    op: Long => Long,
    predicate: Long => Boolean,
    trueMonkey: Int,
    falseMonkey: Int,
    itemsProcessed: Long = 0):
    
    override def toString(): String = s"Monkey $num t->$trueMonkey f-> $falseMonkey count $itemsProcessed items $items"
    
    def round(inThrows: Vector[Queue[Long]]): (Monkey, Vector[Queue[Long]]) =
      val inItems = inThrows(num)
      val inCount = inItems.length
      val newThrows = inItems.foldLeft(inThrows) { (outThrows, item) =>
        val worried = op(item) / 3
        val target = if predicate(worried) then trueMonkey else falseMonkey
        //println(s"$num takes $item, worries ${op(item)}, throws $worried to $target")
        outThrows.updated(
          target,
          outThrows(target).enqueue(worried)
        )
      }
      val updated = this.copy(items = Queue.empty, itemsProcessed = itemsProcessed + inCount)
      (updated, newThrows.updated(num, Queue.empty))

  object Monkey:
    def fromList(params: List[String]): Monkey = params match
      case num :: starts :: op :: oparg :: divarg :: truemonkey :: falsemonkey :: Nil => 
        val items = starts.split(", ?").map(_.toLong).to(Queue)
        val opfun: Long => Long = (op, oparg) match
          case ("*", s) if s == "old"   => n => n * n
          case ("*", s)                 => n => n * s.toInt
          case ("+", s) if s == "old"   => n => n + n
          case ("+", s)                 => n => n + s.toInt
          // case ("/", s) if s == "old"   => n => n / n
          // case ("/", s)                 => n => n / s.toInt
          // case ("-", s) if s == "old"   => n => n - n
          // case ("-", s)                 => n => n - s.toInt
        val pred: Long => Boolean = n => n % divarg.toLong == 0
        Monkey(num.toInt, items, opfun, pred, truemonkey.toInt, falsemonkey.toInt)
        

      case line => throw new IllegalArgumentException(s"Bad line: $line")


  val oneMonkeyRE = raw"Monkey\s+(\d+):\R" +
    raw"\s+Starting items: ([\d, ]+)\R" +
    raw"\s+Operation: new = old (\S+) (\d+|old)\R" +
    raw"\s+Test: divisible by (\d+)\R" +
    raw"\s+If true: throw to monkey (\d+)\R" +
    raw"\s+If false: throw to monkey (\d+)(?:\R|$$)"
  val oneMonkeyRegex = oneMonkeyRE.r

  val input ="""Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

end Day11a
