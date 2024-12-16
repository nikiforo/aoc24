package io.github.nikiforo.aoc24

import scala.collection.immutable.TreeSet

object D16T2 {

  private case class Coord(i: Int, j: Int)

  private sealed trait Dir { def apply(c: Coord): Coord }

  private object Dir {

    case object Up extends Dir { def apply(c: Coord): Coord = Coord(c.i - 1, c.j) }
    case object Right extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j + 1) }
    case object Down extends Dir { def apply(c: Coord): Coord = Coord(c.i + 1, c.j) }
    case object Left extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j - 1) }

    val cwMap: Map[Dir, Dir] = Map(Up -> Right, Right -> Down, Down -> Left, Left -> Up)
    val ccwMap: Map[Dir, Dir] = cwMap.map(_.swap)

    implicit class DirOps(val dir: Dir) extends AnyVal {

      def cw: Dir = cwMap(dir)
      def ccw: Dir = ccwMap(dir)
    }

    val ord: Map[Dir, Int] = Map(Up -> 1, Down -> 2, Left -> 3, Right -> 4)
  }

  private implicit val ordDir: Ordering[Dir] = Ordering.by(Dir.ord)
  private implicit val ordCoord: Ordering[Coord] = Ordering.by(c => (c.i, c.j))

  private implicit class vectorVectorOps(val arr: Vector[Vector[Char]]) extends AnyVal {

    def apply(c: Coord): Char = arr(c.i)(c.j)
    def indices2d: Vector[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toVector
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("16")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toVector.map(_.toVector)
    val start = arr.indices2d.find(c => arr(c) == 'S').get
    val end = arr.indices2d.find(c => arr(c) == 'E').get
    val visited = go(arr, TreeSet((0, start, Dir.Right)), Map((start, Dir.Right) -> (0, Set.empty)))
    val paths = Set(Dir.Right, Dir.Up).map(visited(end, _))
    paths.filter(_._1 == paths.map(_._1).min).flatMap(_._2).size + 1
  }

  private type Visited = Map[(Coord, Dir), (Int, Set[Coord])]
  private type Visit = (Int, Coord, Dir)

  private def go(arr: Vector[Vector[Char]], visit: TreeSet[Visit], visited: Visited): Visited =
    if (visit.isEmpty) visited
    else {
      val (cost, coord, dir) = visit.head
      if (arr(coord) == '#') go(arr, visit.tail, visited)
      else {
        val toVisit = (cost + 1, dir(coord), dir) :: List(dir.cw, dir.ccw).map(d => (cost + 1000, coord, d))
        val (nVisit, nVisited) = toVisit.foldLeft((visit.tail, visited))(fold(visited(coord, dir)._2 + coord))
        go(arr, nVisit, nVisited)
      }
    }

  private def fold(path: Set[Coord])(state: (TreeSet[Visit], Visited), toVisit: Visit): (TreeSet[Visit], Visited) = {
    val ((visit, visited), (cost, coord, dir)) = (state, toVisit)
    visited.get((coord, dir)) match {
      case Some((prevCost, paths)) =>
        (visit, if (cost > prevCost) visited else visited.updated((coord, dir), (cost, paths ++ path)))
      case None => (visit + ((cost, coord, dir)), visited.updated((coord, dir), (cost, path)))
    }
  }
}
