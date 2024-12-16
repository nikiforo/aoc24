package io.github.nikiforo.aoc24

import scala.collection.immutable.TreeSet

object D16T1 {

  case class Coord(i: Int, j: Int)

  sealed trait Dir extends Function1[Coord, Coord]

  object Dir {

    object Up extends Dir { def apply(c: Coord): Coord = Coord(c.i - 1, c.j) }
    object Right extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j + 1) }
    object Down extends Dir { def apply(c: Coord): Coord = Coord(c.i + 1, c.j) }
    object Left extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j - 1) }

    val all = Set(Up, Right, Down, Left)

    val anti = Map(Up -> Down, Down -> Up, Left -> Right, Right -> Left)

    val ord = Map(Up -> 1, Down -> 2, Left -> 3, Right -> 4)
  }

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
    val finish = arr.indices2d.find(c => arr(c) == 'E').get
    go(arr, TreeSet((0, Dir.Right, start)), Set.empty, finish)
  }

  def go(array: Array[Array[Char]], visit: TreeSet[(Int, Dir, Coord)], visited: Set[Coord], finish: Coord): Int = {
    val (cost, dir, coord) = visit.head
    val toVisit = visit.tail + ((1 + cost, dir, dir(coord))) ++ bent(dir).map(d => (1001 + cost, d, d(coord)))
    if (array(coord) == '#' || visited(coord)) go(array, visit.tail, visited + coord, finish)
    else if (coord == finish) cost
    else go(array, toVisit, visited + coord, finish)
  }

  private def bent(dir: Dir): List[Dir] = (Dir.all - dir - Dir.anti(dir)).toList
}
