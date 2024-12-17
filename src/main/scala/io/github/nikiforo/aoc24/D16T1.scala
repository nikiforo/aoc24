package io.github.nikiforo.aoc24

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object D16T1 {

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
    def indices2d: Array[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toArray
  }

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("16")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toVector.map(_.toVector)
    val start = arr.indices2d.find(c => arr(c) == 'S').get
    val finish = arr.indices2d.find(c => arr(c) == 'E').get
    go(arr, TreeSet((0, Dir.Right, start)), Set.empty, finish)
  }

  @tailrec
  def go(array: Vector[Vector[Char]], visit: TreeSet[(Int, Dir, Coord)], visited: Set[Coord], finish: Coord): Int = {
    val (cost, dir, coord) = visit.head
    val bent = List(dir.cw, dir.ccw).map(d => (1001 + cost, d, d(coord)))
    val toVisit = visit.tail + ((1 + cost, dir, dir(coord))) ++ bent
    if (array(coord) == '#' || visited(coord)) go(array, visit.tail, visited + coord, finish)
    else if (coord == finish) cost
    else go(array, toVisit, visited + coord, finish)
  }
}
