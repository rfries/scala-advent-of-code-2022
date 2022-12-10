import Day2b.*
import scala.io.Source

@main def day2bmain(args: String*): Unit =
  println(s">> Hello Day2!")
  val src = Source.fromFile("day2.in.txt")
    .getLines()
    .map(_.trim.split("\\s+"))
  // val src = Source.fromString(Day2b.input)
  //   .getLines()
  //   .map(_.trim.split("\\s+"))

  val rounds = src.map(arr => (arr(0), arr(1)))
    .map { (exp, res) =>
      val shape = shapeForOther(exp)
      val desired = desiredForSelf(res)
      val response = responseForDesiredResult(shape, desired)
      println(s"shape: $shape, desired: $desired, resp: $response")
      (shape, response)
    }

  val scores = rounds
    .map((expShape, resShape) => (expShape.score(resShape) + expShape.score, resShape.score(expShape) + resShape.score))
    .toVector

  println(scores)
  println(s"Totals: Other Player: ${scores.map(_._1).sum}, Self: ${scores.map(_._2).sum}")

object Day2b {

  import Shape.*
  import Desired.*

  enum Shape(
    val score: Int,
    val otherCode: String):

    def score(other: Shape): Int = Shape.scoreTab
      .get(this)
      .flatMap(_.get(other))
      .getOrElse(throw new IllegalStateException("Enum score not found!"))

    case Rock     extends Shape(1, "A")
    case Paper    extends Shape(2, "B")
    case Scissors extends Shape(3, "C")

  object Shape:
    private val scoreTab: Map [Shape, Map[Shape, Int]] =
      Map(
        Rock      ->  Map(Rock -> 3,  Paper -> 0, Scissors -> 6),
        Paper     ->  Map(Rock -> 6,  Paper -> 3, Scissors -> 0),
        Scissors  ->  Map(Rock -> 0,  Paper -> 6, Scissors -> 3)
      )

  enum Desired(val code: String):
    case Lose extends Desired("X")
    case Draw extends Desired("Y")
    case Win  extends Desired("Z")

  object Desired:
    val desiredTab: Map [Shape, Map[Desired, Shape]] =
      Map(
        Rock      -> Map( Draw  -> Rock,  Win   -> Paper,   Lose  -> Scissors),
        Paper     -> Map( Lose  -> Rock,  Draw  -> Paper,   Win   -> Scissors),
        Scissors  -> Map( Win   -> Rock,  Lose  -> Paper,   Draw  -> Scissors)
      )

  def shapeForOther(code: String): Shape = Shape.values
    .find(_.otherCode == code)
    .getOrElse(throw new IllegalArgumentException(s"Invalid code for expected play: $code"))

  def desiredForSelf(code: String): Desired = Desired.values
    .find(_.code == code)
    .getOrElse(throw new IllegalArgumentException(s"Invalid code for desired result: $code"))

  def responseForDesiredResult(shape: Shape, desired: Desired): Shape = Desired.desiredTab
    .get(shape)
    .flatMap(_.get(desired))
    .getOrElse(throw new IllegalStateException(s"Desired result lookup failed for $shape/$desired"))

  val input: String = """A Y
B X
C Z
"""

}
