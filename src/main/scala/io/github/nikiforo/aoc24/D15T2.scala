package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D15T2 {

  implicit class vectorVectorOps(val arr: Vector[Vector[Char]]) extends AnyVal {

    def apply(c: Coord): Char = arr(c.i)(c.j)
    def upd(c: Coord, v: Char): Vector[Vector[Char]] = arr.updated(c.i, arr(c.i).updated(c.j, v))
    def indices2d: Vector[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toVector
    def isBox(c: Coord): Boolean = isBoxLeft(c) || isBoxRight(c)
    def isBoxLeft(c: Coord): Boolean = arr(c) == '['
    def isBoxRight(c: Coord): Boolean = arr(Left(c)) == '['
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

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("15")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val (initState, moves) = parse(lines)
    val initCoord = initState.indices2d.find(initState(_) == '@').get
    val (_, resultState) = moves.map(Dir.from).foldLeft((initCoord, initState)) { case ((coord, state), dir) =>
      val step = dir(coord)
      dir match {
        case Up | Down => if (blockedV(step, state, dir)) (coord, state) else (step, pushV(step, state, dir))
        case Left | Right => if (blockedH(step, state, dir)) (coord, state) else (step, pushH(step, state, dir))
      }
    }
    gps(resultState)
  }

  private def parse(lines: List[String]): (Vector[Vector[Char]], List[Char]) = {
    val scheme = lines.takeWhile(_.nonEmpty)
    val widerScheme = scheme.toVector.map(_.toVector).map(_.flatMap { c =>
      if (c == '#') "##"
      else if (c == 'O') "[]"
      else if (c == '.') ".."
      else "@."
    })
    val moves = lines.drop(scheme.size + 1)
    (widerScheme, moves.mkString("").toList)
  }

  @tailrec private def blockedH(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Boolean =
    state(coord) == '#' || (state.isBox(coord) && blockedH(dir(coord), state, dir))

  private def blockedV(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Boolean = {
    val other = if (state.isBoxLeft(coord)) Right(coord) else Left(coord)
    state(coord) == '#' || state.isBox(coord) && (blockedV(dir(coord), state, dir) || blockedV(dir(other), state, dir))
  }

  private def pushH(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Vector[Vector[Char]] =
    if (!state.isBox(coord)) state
    else pushH(dir(coord), state, dir).upd(dir(coord), state(coord)).upd(coord, '.')

  private def pushV(coord: Coord, state: Vector[Vector[Char]], dir: Dir): Vector[Vector[Char]] =
    if (!state.isBox(coord)) state
    else {
      val other = if (state.isBoxLeft(coord)) Right(coord) else Left(coord)
      val state1 = pushV(dir(other), pushV(dir(coord), state, dir), dir)
      state1.upd(dir(coord), state(coord)).upd(dir(other), state(other)).upd(coord, '.').upd(other, '.')
    }

  def gps(arr: Vector[Vector[Char]]): Long =
    arr.indices2d.map(c => if (arr(c) == '[') 100L * c.i + c.j else 0L).sum
}
