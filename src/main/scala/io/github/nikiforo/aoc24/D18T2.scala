package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D18T2 {

  private case class Coord(i: Int, j: Int) {

    def up: Coord = Coord(i + 1, j)
    def down: Coord = Coord(i - 1, j)
    def left: Coord = Coord(i, j - 1)
    def right: Coord = Coord(i, j + 1)
    def neighbour4: List[Coord] = List(up, down, left, right)
  }

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("18")
    println(compute(lines, 70, 70, 1024))
  }

  def compute(lines: List[String], fi: Int, fj: Int, start: Int): String = {
    val parsed = lines.map(parse)
    val index = (start until Int.MaxValue).find(i => !go(Set(Coord(0, 0)), parsed.take(i).toSet, Coord(fi, fj)))
    parsed(index.get - 1).toString
  }

  @tailrec private def go(visit: Set[Coord], doNotVisit: Set[Coord], finish: Coord): Boolean = {
    def inBorder(c: Coord): Boolean = 0 <= c.i && c.i <= finish.i && 0 <= c.j && c.j <= finish.j
    visit.headOption match {
      case None => false
      case Some(coord) =>
        if (coord == finish) true
        else if (doNotVisit(coord)) go(visit.tail, doNotVisit, finish)
        else go(visit.tail ++ coord.neighbour4.filter(inBorder), doNotVisit + coord, finish)
    }
  }

  private def parse(line: String) = line match { case s"$i,$j" => Coord(i.toInt, j.toInt) }
}
