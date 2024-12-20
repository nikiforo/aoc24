package io.github.nikiforo.aoc24

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object D20T1 {

  private case class Coord(i: Long, j: Long) {
    def neighbour4: List[Coord] = List((i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)).map(p => Coord(p._1, p._2))
  }

  private implicit val ordCoord: Ordering[Coord] = Ordering.by(c => (c.i, c.j))

  private implicit val ordList: Ordering[List[(Int, Coord)]] = Ordering.by(_.headOption)

  private implicit class vectorVectorOps(val arr: Vector[Vector[Char]]) extends AnyVal {

    def apply(c: Coord): Char = arr(c.i.toInt)(c.j.toInt)
    def indices2d: Array[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toArray
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("20")
    println(compute(lines, 100))
  }

  def compute(lines: List[String], save: Int): Int = {
    val arr = lines.toVector.map(_.toVector)
    val start = arr.indices2d.find(c => arr(c) == 'S').get
    val finish = arr.indices2d.find(c => arr(c) == 'E').get
    val noCheat = go(arr, TreeSet((0, start, List((0, start)))), Set.empty, finish)
    goCheat(noCheat.toVector, save)
  }

  @tailrec
  private def go(
    arr: Vector[Vector[Char]],
    visit: TreeSet[(Int, Coord, List[(Int, Coord)])],
    visited: Set[Coord],
    finish: Coord,
  ): List[(Int, Coord)] = {
    val (cost, coord, path) = visit.head
    val toVisit = visit.tail ++ coord.neighbour4.map(c => (cost + 1, c, (cost + 1, c) :: path))
    if (arr(coord) == '#' || visited(coord)) go(arr, visit.tail, visited + coord, finish)
    else if (coord == finish) path
    else go(arr, toVisit, visited + coord, finish)
  }

  private def goCheat(maxs: Vector[(Int, Coord)], save: Int): Int =
    maxs.indices.map { i =>
      val iNeighbours = maxs(i)._2.neighbour4.flatMap(_.neighbour4).toSet
      ((i + 1) until maxs.length).count { j =>
        iNeighbours(maxs(j)._2) && (maxs(i)._1 - maxs(j)._1 - 2 >= save)
      }
    }.sum
}
