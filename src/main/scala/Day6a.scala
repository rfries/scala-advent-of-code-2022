import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scala.io.Source
object Day6a:
  val markerSize = 4

  // val input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  val input = Source.fromFile("day6.in.txt").mkString

  def main(args: Array[String]): Unit =
    println("Hello Day6a!")
    println(s"input $input")

    @tailrec
    def findMarker(off: Int, buf: String, seen: Queue[Char]): Int =
        buf.headOption match
          case None => throw new IllegalStateException("No marker found!")
          case Some(ch) =>
            if seen.length >= markerSize - 1
              && !seen.contains(ch) 
              && seen.distinct.length == seen.length then

              println(s"off $off got '$ch' no match $seen")
              off
            else
              println(s"off $off got '$ch' too short or dup $seen")
              findMarker(off + 1, buf.tail,  seen.prepended(ch).take(markerSize-1))

    val numChars = findMarker(0, input, Queue.empty) + 1
    println(s"Result: Start of packet detected after $numChars characters")
