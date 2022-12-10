import Day3b.*
import scala.io.Source

@main def day3bmain(args: String*): Unit =
  println(s">> Hello Day3!")

  val src = Source.fromFile("day3.in.txt")
    .getLines()
    .map(_.trim)

  val groups = src.sliding(3, 3).toIndexedSeq

  println(groups)

  val badges = groups.map(findBadges)

  val scores = badges.map(Day3b.score)
  println(s"total: ${scores.sum}")

object Day3b {

  def score(item: Char): Int = item match
    case c if c.isUpper => item - 'A' + 27
    case c if c.isLower => item - 'a' + 1
    case c => throw new IllegalArgumentException(s"Bad char: '$c'")

  def findBadges(members: Seq[String]): Char =
    println(s">>> $members")
    require(members.size == 3)
    val intersect = members.reduce((a, b) => a.toSet.intersect(b.toSet).mkString)
    require(intersect.size == 1)
    intersect(0)

  val input: String = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

}
