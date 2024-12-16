package io.github.nikiforo.aoc24

import scala.collection.immutable.TreeSet

object D16T2 {

  case class Coord(i: Int, j: Int)

  sealed trait Dir { def apply(c: Coord): Coord }

  object Dir {

    case object Up extends Dir { def apply(c: Coord): Coord = Coord(c.i - 1, c.j) }
    case object Right extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j + 1) }
    case object Down extends Dir { def apply(c: Coord): Coord = Coord(c.i + 1, c.j) }
    case object Left extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j - 1) }

    val all: Set[Dir] = Set(Up, Right, Down, Left)

    val anti: Map[Dir, Dir] = Map(Up -> Down, Down -> Up, Left -> Right, Right -> Left)

    val ord: Map[Dir, Int] = Map(Up -> 1, Down -> 2, Left -> 3, Right -> 4)
  }

  type Visit = (Int, Coord, Dir)
  type Visited = Map[(Coord, Dir), (Int, Set[Dir])]

  private implicit val ordDir: Ordering[Dir] = Ordering.by(Dir.ord)

  private implicit val ordCoord: Ordering[Coord] = Ordering.by(c => (c.i, c.j))

  implicit class vectorVectorOps(val arr: Array[Array[Char]]) extends AnyVal {

    def apply(c: Coord): Char = arr(c.i)(c.j)
    def indices2d: Array[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toArray
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("16")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    val start = arr.indices2d.find(c => arr(c) == 'S').get
    val end = arr.indices2d.find(c => arr(c) == 'E').get
    val visited = go(arr, TreeSet((0, start, Dir.Right)), Map.empty)
    backtrack(visited, start, Set((end, Dir.Left)), Set.empty).size
  }

  private def backtrack(visited: Visited, start: Coord, set: Set[(Coord, Dir)], acc: Set[Coord]): Set[Coord] =
    if (set.isEmpty) acc
    else {
      val next =
        set.flatMap { case (coord, dir) =>
          val c = dir(coord)
          if (coord == start) Set.empty
          else visited(dir(coord), Dir.anti(dir))._2.map((c, _))
        }
      backtrack(visited, start, next, acc ++ set.map(_._1))
    }

  def go(arr: Array[Array[Char]], visit: TreeSet[Visit], seen: Visited): Visited =
    if (visit.isEmpty) seen
    else {
      val (cost, coord, dir) = visit.head
      if (arr(coord) == '#') go(arr, visit.tail, seen)
      else {
        val toVisit = (cost + 1, dir(coord), dir) :: bent(dir).map(d => (cost + 1001, dir(coord), d))
        val (nVisit, nVisited) = toVisit.foldLeft((visit.tail, seen))(foldEval(Dir.anti(dir)))
        go(arr, nVisit, nVisited)
      }
    }

  private def foldEval(from: Dir)(state: (TreeSet[Visit], Visited), toVisit: Visit): (TreeSet[Visit], Visited) = {
    val ((visit, visited), (cost, coord, dir)) = (state, toVisit)
    visited.get((coord, dir)) match {
      case Some((prevCost, fromDirs)) =>
        if (cost > prevCost) (visit, visited)
        else if (fromDirs(from)) (visit, visited)
        else (visit + toVisit, visited.updated((coord, dir), (cost, fromDirs + from)))
      case None => (visit + toVisit, visited.updated((coord, dir), (cost, Set(from))))
    }
  }

  private def bent(dir: Dir): List[Dir] = (Dir.all - dir - Dir.anti(dir)).toList
}
