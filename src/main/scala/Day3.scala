import Day3.*
import scala.io.Source

@main def day3main(args: String*): Unit =
  println(s">> Hellooo Day3!")
  val src = Source.fromFile("day3.in.txt")
    .getLines()
    .map(_.trim)
  // val src = Source.fromString(Day3.input)
  //   .getLines()
  //   .map(_.trim)
  val intersects = src.toVector.map { line =>
    require(line.length > 0 && line.length % 2 == 0)
    val sublen = line.length / 2
    val (bucket1, bucket2) = (line.take(sublen), line.takeRight(sublen))
    val intersect = bucket1.toSet.intersect(bucket2.toSet)
    require(intersect.size == 1)
    intersect.head
  }

  val scores = intersects.map(Day3.score)


  println(intersects)
  //println(s"scores: $scores")
  println(s"total: ${scores.sum}")


object Day3 {

  def score(item: Char): Int = item match
    case c if c.isUpper => item - 'A' + 27
    case c if c.isLower => item - 'a' + 1
    case c => throw new IllegalArgumentException(s"Bad char: '$c'")

  val input: String = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

}
