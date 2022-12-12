import scala.io.Source
import scala.collection.mutable.Queue
import scala.annotation.newMain

object Day11bMutLong:

  def main(args: Array[String]): Unit =
    //val text = Source.fromString(input).getLines().mkString("\n")
    val text = Source.fromFile("day11.in.txt").getLines().mkString("\n")

    println(s"Monkey Business (10000 rounds): ${monkeyBusiness(text, 10000)}")
  end main

  def monkeyBusiness(text: String, rounds: Int): Long =
    val afterMonkeys = after(text, rounds).map(_.itemsProcessed).sorted
    afterMonkeys.takeRight(2).product

  def after(text: String, rounds: Int): Array[Monkey] =
    val monkeys = getMutableMonkeys(text)
    val coprimes = monkeys.map(_.divBy).product
    (0 until rounds).map { round =>
      monkeys.foreach( monkey => monkey.turn(monkeys, coprimes))
    }
    monkeys
  
  def getMutableMonkeys(text: String): Array[Monkey] =
    oneMonkeyRegex.findAllMatchIn(text)
      .map(_.subgroups)
      .map(Monkey.fromList)
      .toArray

  class Monkey(num: Int,
    val items: Queue[Long],
    val op: Long => Long,
    val predicate: Long => Boolean,
    val divBy: Long,
    val trueMonkey: Int,
    val falseMonkey: Int,
    var itemsProcessed: Long = 0):
    
    override def toString(): String = s"[Monkey $num, total $itemsProcessed, t->$trueMonkey f-> $falseMonkey, Q(${items})]"
    
    def turn(monkeys: Array[Monkey], coprimes: Long): Unit =
      itemsProcessed += items.length
      items.foreach { item =>
        val worried = op(item) % coprimes
        val target = if predicate(worried) then trueMonkey else falseMonkey
        monkeys(target).items.enqueue(worried)
      }
      items.clear


  object Monkey:
    def fromList(params: List[String]): Monkey = params match
      case num :: starts :: op :: oparg :: divarg :: truemonkey :: falsemonkey :: Nil => 
        val items = starts.split(", ?").map(_.toLong).to(Queue)
        val opfun: Long => Long = (op, oparg) match
          case ("*", s) if s == "old"   => n => n * n
          case ("*", s)                 => { val i = s.toLong; n => n * i }
          case ("+", s) if s == "old"   => n => n + n
          case ("+", s)                 => { val i = s.toLong; n => n + i }
        val divBy = divarg.toLong
        val pred: Long => Boolean = { n => n % divBy == 0 }
        Monkey(num.toInt, items, opfun, pred, divBy, truemonkey.toInt, falsemonkey.toInt)

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

end Day11bMutLong
