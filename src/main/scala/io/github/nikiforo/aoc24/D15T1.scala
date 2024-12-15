package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D15T1 {

  implicit class vectorVectorOps(val arr: Vector[Vector[Char]]) extends AnyVal {

    def apply(c: Coord): Char = arr(c.i)(c.j)
    def upd(c: Coord, v: Char): Vector[Vector[Char]] = arr.updated(c.i, arr(c.i).updated(c.j, v))
    def indices2d: Vector[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toVector
    def isBox(c: Coord): Boolean = arr(c) == 'O'
  }

  case class Coord(i: Int, j: Int)

  sealed trait Dir extends Function1[Coord, Coord]
  object Up extends Dir { def apply(c: Coord): Coord = Coord(c.i - 1, c.j) }
  object Right extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j + 1) }
  object Down extends Dir { def apply(c: Coord): Coord = Coord(c.i + 1, c.j) }
  object Left extends Dir { def apply(c: Coord): Coord = Coord(c.i, c.j - 1) }

  object Dir {

    def from(c: Char): Dir =
      if (c == '^') Up
      else if (c == '>') Right
      else if (c == 'v') Down
      else Left
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("15")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val (initState, moves) = parse(lines)
    val initCoord = initState.indices2d.find(initState(_) == '@').get
    val (_, resultState) = moves.map(Dir.from).foldLeft((initCoord, initState)) { case ((coord, state), dir) =>
      val step = dir(coord)
      if (isBlocked(step, state, dir)) (coord, state)
      else (step, push(step, state, dir))
    }
    gps(resultState)
  }

  private def parse(lines: List[String]): (Vector[Vector[Char]], List[Char]) = {
    val scheme = lines.takeWhile(_.nonEmpty)
    val moves = lines.drop(scheme.size + 1)
    (scheme.map(_.toVector).toVector, moves.mkString("").toList)
  }

  @tailrec private def isBlocked(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Boolean =
    state(coord) == '#' || (state.isBox(coord) && isBlocked(dir(coord), state, dir))

  private def push(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Vector[Vector[Char]] =
    if (!state.isBox(coord)) state
    else push(dir(coord), state, dir).upd(dir(coord), state(coord)).upd(coord, '.')

  def gps(arr: Vector[Vector[Char]]): Long =
    arr.indices2d.map(c => if (arr(c) == 'O') 100L * c.i + c.j else 0L).sum
}
