import scala.collection.mutable.{ArraySeq => BadMutable}
import scala.annotation.tailrec
import scala.io.Source
object Day8a:

  def main(args: Array[String]): Unit =
    println("Hello Day8a!")
    //val lines = Source.fromString(input).getLines()
    val lines = Source.fromFile("day8.in.txt").getLines()
    val gridlines = lines.map(_.toVector.map(ch => (ch - '0').toInt)).toVector
    val side = gridlines(0).length
    val grid = gridlines.flatten
    val vis = BadMutable.fill(side*side)(false)
   
    val northSide = Range(  0,                      side,             1)
    val southSide = Range(  (side-1) * side,        side * side,      1)
    val eastSide  = Range(  side-1,                 side * side,      side)
    val westSide  = Range(  0,                      side * side,      side) // weird, but works

    visibleGrid(grid, side, vis)
    println(vis.count(_ == true))

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