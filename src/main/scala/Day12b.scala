import scala.io.Source
import scala.annotation.newMain
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.math.Ordering.Reverse
import scala.collection.mutable.Queue
import scala.collection.mutable

object Day12b:

  def main(args: Array[String]): Unit =
    //val lines = Source.fromString(input).getLines().toVector
    val lines = Source.fromFile("day12.in.txt").getLines().toVector

    val (startPos, endPos) = findStartAndEnd(lines)
    println(s"Starting position (x, y): $startPos")
    println(s"Ending position (x, y):   $endPos")

    val heights = lines.map(line =>
      line.replace("S", "a").replace("E", "z").map(c => c - 'a')
    )
    require(heights.map(_.length).toSet.size == 1)
    val numRows = heights.length
    val numCols = heights(0).length
    println(s"The map is ${numRows} rows by ${numCols} columns (${numRows * numCols}).")

    val cells: Vector[Vector[Cell]] =  heights.zipWithIndex.map { (hline, y) =>
      (0 until hline.length).map(x => Cell(Pos(x, y), heights(y)(x))).toVector
    }

    val potentialEndPoints: Vector[(Int, Vector[Pos])] = chooseEndPoints(cells)
    val starts = potentialEndPoints.find((height, _) => height == 0)
    println(s"Number of starts: ${starts.get._2.length}")
    val path = starts.foldLeft(Option.empty[Vector[Pos]]) {
      case (pathOpt, (height, posList)) =>
        pathOpt orElse {
          val paths = posList.map(pos => findShortest(cells, pos, endPos))
          val best = paths.filter(_.nonEmpty).sortBy(_.length)
          best.headOption
        }
    }
   
    path match
      case Some(res)  => println(s"Found route with ${res.length} steps to ${res.last}.\n$path")
      case None       => println(s"No path.")
    
  end main

  def chooseEndPoints(cells: Vector[Vector[Cell]]): Vector[(Int, Vector[Pos])] =
    cells.flatten.map(cell => (cell.height, cell.pos))
      .groupBy(_._1)
      .mapValues(posVec => posVec.map(_._2))
      .toVector
      .sortBy(_._1)(Ordering[Int].reverse)

  def updatePath(toVisit: PriorityQueue[Path], path: Path, newVia: Path, cost: Double): Unit =
    val paths = toVisit.dequeueAll
    val newPaths = paths.indexWhere(p => p.pos == path.pos) match
      case n if n >= 0 => paths.updated(n, path.copy(via = Some(newVia), cost = cost))
      case _ => throw new IllegalStateException(s"Updated path not found for path $path")
    toVisit.enqueue(newPaths*)
    require(toVisit.exists(p => p.pos == path.pos))

  def removePath(toVisit: PriorityQueue[Path], path: Path): Unit =
    toVisit.enqueue(toVisit.dequeueAll.filterNot(p => p.pos == path.pos) *)

  def findShortest(cells: Vector[Vector[Cell]], startPos: Pos, endPos: Pos): Vector[Pos] =
    val neighbors = findNeighbors(cells)
    val toVisit: PriorityQueue[Path] = PriorityQueue.empty(Path.ord_Path)
    //val visited: Queue[(Pos, Double)] = Queue.empty

    print(s">> searching for route from $startPos to $endPos...")

    toVisit.addAll(
      cells.flatten.map(cell =>
        Path(cell.pos, if (cell.pos == startPos) 0.0 else Double.PositiveInfinity, None))
    )

    var found: Vector[Pos] = Vector.empty
    while (toVisit.nonEmpty && found.isEmpty) {
      val cur = toVisit.dequeue
      //visited.enqueue((cur.pos, cur.cost))

      //if cur.cost == Double.PositiveInfinity then println (s">> ***** processing path with no route: $cur")

      if cur.cost != Double.PositiveInfinity then
        // find all neighbors for the current cell
        neighbors.get(cur.pos).foreach { dirMap =>

          dirMap.foreach { (direction, neighbor) =>
            // find all the neighbors which still need to be visited
            toVisit.find(_.pos == neighbor.pos) match
              case Some(npath) =>
                // neighbor is in the "toVisit" queue    
                val cost = cur.cost + 1

                if npath.pos == endPos then
                  // Found it!
                  found = cur.fullPath :+ npath.pos
                  
                else if cost < npath.cost then
                  // the cost through us to the neighbor is lower than the neighbor's cost, so
                  // update them to go through us
                  updatePath(toVisit, npath, cur, cost)

              case None =>
                // neighbor already visited
          }
        }
    }
    println(s"path len ${found.length}")
    found
  end findShortest

  def renderMapWithVisited(visited: Queue[(Pos, Double)], cells: Vector[Vector[Cell]]): String =
    val posToCost = visited.toMap
    cells.map { row =>
      row.map { cell => 
        posToCost.get(cell.pos) match
          case Some(cost) => if cost == Double.PositiveInfinity then '*' else '#'
          case None => (cell.height + 'a').toChar
        }.mkString
    }.mkString("\n")

  def findNeighbors(cells: Vector[Vector[Cell]]): Map[Pos, Map[Direction, Cell]] =
    import Direction.*

    def forCell(cell: Cell): Map[Direction, Cell] =
      Direction.values.map(dir => (dir, forDirection(dir, cell.pos)))
        .collect { case (dir, Some(neighbor)) => (dir, neighbor) }
        .filter((dir, neighbor) => neighbor.height <= cell.height + 1)
        .toMap
    
    def forDirection(dir: Direction, from: Pos): Option[Cell] = dir match
      case North => if from.y == 0                        then None else Some(cells(from.y - 1)(from.x))
      case South => if from.y >= cells.length - 1         then None else Some(cells(from.y + 1)(from.x))
      case West  => if from.x == 0                        then None else Some(cells(from.y)(from.x - 1))
      case East  => if from.x >= cells(from.y).length - 1 then None else Some(cells(from.y)(from.x + 1))
    
    cells.flatten.foldLeft(Map.empty) { (map, cell) =>
      map + ((cell.pos, forCell(cell)))
    }
    
  def printPriorityQueue(q: PriorityQueue[Path]): Unit =
    val all = q.dequeueAll
    println(s"${all.take(5)}...")
    q.enqueue(all*)

  case class Pos(x: Int, y: Int)

  case class Cell(pos: Pos, height: Int)

  case class Path(pos: Pos, cost: Double, via: Option[Path]):
    def fullPath: Vector[Pos] =
      @tailrec
      def nextPath(next: Path, paths: Vector[Path]): Vector[Path] =
        next.via match
          case Some(via) => nextPath(via, next +: paths)
          case None =>  paths
      nextPath(this, Vector.empty).map(_.pos)
    override def toString(): String = s"Path($pos, $cost, ${via.map(_.pos.toString).getOrElse("<no path>")})"
    
  object Path:
    given ord_Path: Ordering[Path] = Ordering.by((p: Path) => p.cost).reverse

  enum Direction:
    case North, South, East, West
  
  case class Edge(pos: Pos, cost: Int)
  case class Grid(heights: Vector[Vector[Int]])

  def findStartAndEnd(lines: Vector[String]): (Pos, Pos) =
    val indexed = lines.zipWithIndex
    val (startOpt, endOpt):(Option[Pos], Option[Pos]) = indexed.foldLeft[(Option[Pos], Option[Pos])]((None, None)) { 
      case ((startOpt, endOpt), (line, lineIdx)) =>
        val newStart = startOpt.orElse {
          val idx = line.indexOf("S")
          if idx < 0 then None else Some(Pos(idx, lineIdx))
        }
        val newEnd = endOpt.orElse {
          val idx = line.indexOf("E")
          if idx < 0 then None else Some(Pos(idx, lineIdx))
        }
        (newStart, newEnd)
    }
    (startOpt, endOpt) match
      case (Some(startPos), Some(endPos)) => (startPos, endPos)
      case _ => throw new IllegalArgumentException("Failed to parse start/end from map.")

  val input ="""Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

end Day12b
