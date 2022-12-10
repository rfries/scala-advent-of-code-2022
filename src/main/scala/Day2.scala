import Day2.*
import scala.io.Source

@main def day2main(args: String*): Unit =
  println(s">> Hello Day2!")
  val src = Source.fromFile("day2.in.txt")
    .getLines()
    .map(_.trim.split("\\s+"))
  // val src = Source.fromString(Day2.input)
  //   .getLines()
  //   .map(_.trim.split("\\s+"))

  val rounds = src.map(arr => (arr(0), arr(1)))
    .map((exp, res) => (shapeForOther(exp), shapeForSelf(res)))

  //println(rounds.toVector)
  val scores = rounds
    .map((expShape, resShape) => (expShape.score(resShape) + expShape.score, resShape.score(expShape) + resShape.score))
    .toVector

  println(scores)
  println(s"Totals: Other Player: ${scores.map(_._1).sum}, Self: ${scores.map(_._2).sum}")

object Day2 {

  import Shape.*

  enum Shape(
    val score: Int,
    val otherCode: String,
    val selfCode: String):

    def score(other: Shape): Int = Shape.scoreTab
      .get(this)
      .flatMap(_.get(other))
      .getOrElse(throw new IllegalStateException("Enum score not found!"))

    case Rock     extends Shape(1, "A", "X")
    case Paper    extends Shape(2, "B", "Y")
    case Scissors extends Shape(3, "C", "Z")

  object Shape:
    private val scoreTab: Map [Shape, Map[Shape, Int]] =
      Map(
        Rock      ->  Map(Rock -> 3,  Paper -> 0, Scissors -> 6),
        Paper     ->  Map(Rock -> 6,  Paper -> 3, Scissors -> 0),
        Scissors  ->  Map(Rock -> 0,  Paper -> 6, Scissors -> 3)
      )


  def shapeForOther(code: String): Shape = Shape.values
    .find(_.otherCode == code)
    .getOrElse(throw new IllegalArgumentException(s"Invalid code for expected play: $code"))

  def shapeForSelf(code: String): Shape = Shape.values
    .find(_.selfCode == code)
    .getOrElse(throw new IllegalArgumentException(s"Invalid code for response play: $code"))

  val input: String = """A Y
B X
C Z
"""

}
