import scala.io.Source
import scala.collection.immutable.Queue
import scala.annotation.newMain
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.math.Ordering.Reverse

object Day12a:

  def main(args: Array[String]): Unit =
    val lines = Source.fromString(input).getLines().toVector
    //val lines = Source.fromFile("day12.in.txt").getLines().toVector

    val (startPos, endPos) = findStartAndEnd(lines)
    println(s"Starting position (x, y): $startPos")
    println(s"Ending position (x, y):   $endPos")

    val heights = lines.map(line =>
      line.replace("S", "a").replace("E", "z").map(c => c - 'a')
    )
    require(heights.map(_.length).toSet.size == 1)

    val cells: Vector[Vector[Cell]] =  heights.zipWithIndex.map { (hline, y) =>
      (0 until hline.length).map(x => Cell(Pos(x, y), heights(y)(x))).toVector
    }

    val path = findShortest(cells, startPos, endPos)
    println(s"Found route with ${path.length} steps.")
    println(path)
    
  end main

  def updatePath(toVisit: PriorityQueue[Path], path: Path, newVia: Path, cost: Double): Unit =
    val paths = toVisit.dequeueAll
    val newPaths = paths.indexWhere(p => p.pos == path.pos) match
      case n if n >= 0 => paths.updated(n, path.copy(via = Some(newVia), cost = cost))
      case _ => throw new IllegalStateException(s"Updated path not found for path $path")
    toVisit.enqueue(newPaths*)

  def removePath(toVisit: PriorityQueue[Path], path: Path): Unit =
    toVisit.enqueue(toVisit.dequeueAll.filterNot(p => p.pos == path.pos) *)

  def findShortest(cells: Vector[Vector[Cell]], startPos: Pos, endPos: Pos): Vector[Pos] =
    val neighbors = findNeighbors(cells)
    val toVisit: PriorityQueue[Path] = PriorityQueue.empty(Path.ord_Path)

    toVisit.addAll(
      cells.flatten.map(cell =>
        Path(cell.pos, if (cell.pos == startPos) 0.0 else Double.PositiveInfinity, None))
    )
    
    var found: Vector[Pos] = Vector.empty
    while (toVisit.nonEmpty && found.isEmpty) {
      val cur = toVisit.dequeue
      //if (cur.cost != Double.PositiveInfinity) (s">> processing path with route: $cur")
      require(cur.cost != Double.PositiveInfinity)
      //println(s">> visiting $cur: ")
      if cur.cost != Double.PositiveInfinity then
        // find all neighbors for the current cell
        neighbors.get(cur.pos).foreach { dirMap =>
          dirMap.foreach { (direction, neighbor) =>
            toVisit.find(_.pos == neighbor.pos) match
              case Some(npath) =>
                // neighbor is in the "toVisit" queue    
                val cost = cur.cost + 1
                if npath.pos == endPos then //&& cur.cost != Double.PositiveInfinity then
                  // Found it!
                  found = cur.fullPath :+ npath.pos
                  println(s">> Found endPos from $cur")

                else if cost < npath.cost then
                  // the cost through us to the neighbor is lower than the neighbor's cost, so
                  // update them to go through us

                  // if npath.pos.y % 10 == 0 then println(s">> updating $npath to $cost (ours ${cur.cost})")
                  //if npath.pos.y % 10 == 0 then printPriorityQueue(toVisit)

                  updatePath(toVisit, npath, cur, cost)

                // else
                //   println(s">> neighbor in toVisit and not updated: $npath")
              case None =>
                // neighbor already visited
          }
        }
      // else
      //   println(">> no path, skipping...")
        // println(s">> done processing $cur, (already dequeued)")
        // removePath(toVisit, cur)
    }
    println("!")
    found

  def findNeighbors(cells: Vector[Vector[Cell]]): Map[Pos, Map[Direction, Cell]] =
    import Direction.*

    def forCell(cell: Cell): Map[Direction, Cell] =
      Direction.values.map(dir => (dir, forDirection(dir, cell.pos)))
        .collect { case (dir, Some(neighbor)) => (dir, neighbor) }
        .filter((dir, neighbor) => Math.abs(cell.height - neighbor.height) <= 1)
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

end Day12a
