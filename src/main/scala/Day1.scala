import scala.io.Source
import sys.process.*
import Day1.*

@main def day1main(args: String*): Unit =
  println(s">> ${"pwd".!!}")
  val lines = Source
    .fromFile("in.txt")
    .getLines
    .toVector
  val efoods = next(lines, Vector.empty, Vector.empty)
  efoods.foreach(println)
  val max = efoods.maxBy(ef => ef.foods.sum)
  println(s"*** MAX: $max ==> ${max.foods.sum}")
  val top3 = efoods
    .map(ef => ef.foods.sum)
    .sorted
    .takeRight(3)
  println(s"Top 3: $top3")
  println(s"Sum of top 3: ${top3.sum}")

object Day1:

  val input: String =
    """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

  case class ElfFood(elfNum: Int, foods: Vector[Int])

  def next(lines: Vector[String], foods: Vector[ElfFood], curFood: Vector[Int]): Vector[ElfFood] =
    if lines.isEmpty then
      if curFood.nonEmpty then
        foods.appended(ElfFood(foods.length, curFood))
      else
        foods
    else
      (lines.head, lines.tail) match
        case (head, tail) if head.isBlank =>
          if curFood.nonEmpty then
            next(tail, foods.appended(ElfFood(foods.length, curFood)), Vector.empty)
          else
            next(tail, foods, curFood)
        case (head, tail) => head.toIntOption match
          case None => throw new IllegalArgumentException(s"Non-integer on input: $head")
          case Some(n) => next(tail, foods, curFood.appended(n))
