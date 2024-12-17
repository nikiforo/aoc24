package io.github.nikiforo.aoc24

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object D18T1 {

  private case class Coord(i: Int, j: Int) {

    def up: Coord = Coord(i + 1, j)
    def down: Coord = Coord(i - 1, j)
    def left: Coord = Coord(i, j - 1)
    def right: Coord = Coord(i, j + 1)
    def neighbour4: List[Coord] = List(up, down, left, right)
  }

  private implicit val ordCoord: Ordering[Coord] = Ordering.by(c => (c.i, c.j))
  private implicit val listOrd: Ordering[List[Coord]] = Ordering.by(_.headOption)

  def main(args: Array[String]): Unit = {
    val lines = aocLines("18")
    println(compute(lines, 70, 70, 1024))
  }

  def compute(lines: List[String], i: Int, j: Int, take: Int): Long =
    go(TreeSet((0, Coord(0, 0), List.empty)), lines.map(parse).take(take).toSet, Coord(i, j))

  @tailrec private def go(visit: TreeSet[(Int, Coord, List[Coord])], doNotVisit: Set[Coord], finish: Coord): Int = {
    def inBorder(c: Coord): Boolean = 0 <= c.i && c.i <= finish.i && 0 <= c.j && c.j <= finish.j
    val (cost, coord, path) = visit.head
    if (coord == finish) path.size
    else if (doNotVisit(coord)) go(visit.tail, doNotVisit, finish)
    else {
      val toVisit = visit.tail ++ coord.neighbour4.filter(inBorder).map(c => (cost + 1, c, c :: path))
      go(toVisit, doNotVisit + coord, finish)
    }
  }

  private def parse(line: String) = line match { case s"$i,$j" => Coord(i.toInt, j.toInt) }
}
