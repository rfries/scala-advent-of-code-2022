import scala.collection.mutable.{ArraySeq => BadMutable}
import scala.annotation.tailrec
import scala.io.Source
object Day8b:

  def main(args: Array[String]): Unit =
    println("Hello Day8b!")
    //val lines = Source.fromString(input).getLines()
    val lines = Source.fromFile("day8.in.txt").getLines()
    val gridlines = lines.map(_.toVector.map(ch => (ch - '0').toInt)).toVector
    val side = gridlines(0).length
    val grid = gridlines.flatten
    val scores = BadMutable.fill(side*side)(1)
   
    calculateScores(grid, side, scores)

    val max = scores.zipWithIndex.maxBy(_._1)

    println(s"Max score: ${max._1} (tree ${max._2})")

  def rangeNorth(n: Int, side: Int): Range = n to      n - (n / side) * side         by -side
  def rangeSouth(n: Int, side: Int): Range = n to      (side-1) * side + (n % side)  by side
  def rangeEast(n: Int, side: Int):  Range = n until   n + (side - (n % side))       by 1
  def rangeWest(n: Int, side: Int):  Range = n to      n - (n % side)                by -1

  def calculateScores(grid: Vector[Int], side: Int, scores: BadMutable[Int]): Unit =
    0 until grid.size foreach { n =>
      scoreRange(rangeNorth(n, side), grid, scores)
      scoreRange(rangeSouth(n, side), grid, scores)
      scoreRange(rangeEast(n, side), grid, scores)
      scoreRange(rangeWest(n, side), grid, scores)
    }
  
  def scoreRange(range: Range, grid: Vector[Int], scores: BadMutable[Int]): Unit =
      @tailrec
      def next(vals: Vector[(Int, Int)], ourHeight: Int, score: Int): Int =
        vals match
          case (n, height) +: tail =>
            if height >= ourHeight then
              score + 1
            else
              next(tail, ourHeight, score + 1)
          case _ => score

      val rangeVals = range.map(n => (n, grid.apply(n))).toVector
      val n = rangeVals.head._1
      val score = next(rangeVals.tail, grid(n), 0)
      val curScore = scores(n)
      scores.update(n, curScore * score)

    // visibleGrid(grid, side, vis)
    // println(vis.count(_ == true))

    /*
    def countVisible(range: Range, edge: Int, grid: Vector[Int]): Int = ???

    def visibleGrid(grid: Vector[Int], side: Int, viz: BadMutable[Boolean]): Unit =
      visibleRanges(northSide, side, grid, viz)
      visibleRanges(southSide, -side, grid, viz)
      visibleRanges(eastSide, -1, grid, viz)
      visibleRanges(westSide, 1, grid, viz)

    def visibleRanges(starts: Range, step: Int, grid: Vector[Int], vis: BadMutable[Boolean]): Unit =
      starts.foreach { start =>
        visibleRange(Range(start, start + (step * side), step), grid, vis)
      }

    def visibleRange(range: Range, grid: Vector[Int], vis: BadMutable[Boolean]): Unit =
      @tailrec
      def next(vals: Vector[(Int, Int)], heighest: Int): Unit =
        vals match
          case (n, height) +: tail =>
            if height > heighest || vals.size == range.size then
              vis.update(n, true)
              next(tail, height)
            else
              next(tail, heighest)
          case _ => ()

      val rangeVals = range.map(n => (n, grid.apply(n))).toVector
      next(rangeVals, 0)

      */
  val input = """30373
25512
65332
33549
35390
"""

/*
    def countVisibleRange(range: Range, edge: Int, grid: Vector[Int]): Int =
      @tailrec
      def next(trees: Vector[Int], highest: Int, vis: Int): Int =
        trees match
          case tree +: tail =>
            if tree > highest then
              next(tail, tree, vis + 1)
            else
              next(tail, highest, vis)
          case _ => vis
      val heights = range.map(grid.apply).toVector
      next(heights, edge, 0)


 


    // def something(): Unit =
    //   @tailrec
    //   def next(trees: Vector[Int], highest: Int, vis: Vector[Boolean]): Int =
    //     trees match
    //       case tree +: tail =>
    //         if tree > highest then
    //           next(tail, tree, vis + 1)
    //         else
    //           next(tail, highest, vis)
    //       case _ => vis
    //   val heights = range.map(grid.apply).toVector
    //   next(heights, edge, 0)
    //   ???
    
    
    
    // def checkVisibleRange(range: Range, edge: Int, grid: Vector[Int]): Vector[Boolean] =
    //   @tailrec
    //   def next(trees: Vector[Int], highest: Int, vis: Int): Int =
    //     trees match
    //       case tree +: tail =>
    //         if tree > highest then
    //           next(tail, tree, vis + 1)
    //         else
    //           next(tail, highest, vis)
    //       case _ => vis
    //   val heights = range.map(grid.apply).toVector
    //   next(heights, edge, 0)
    
    

*/