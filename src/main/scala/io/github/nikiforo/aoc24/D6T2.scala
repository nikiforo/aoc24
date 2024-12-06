package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D6T2 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("6")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toVector.map(_.toVector)
    val (one, other) = initial(arr)
    (for {
      i <- arr.indices
      j <- arr.indices
    } yield
      if (arr(i)(j) != '.') 0
      else walk(one, other, -1, 0, arr.updated(i, arr(i).updated(j, '#')), Set.empty)).sum
  }

  private def initial(arr: Vector[Vector[Char]]): (Int, Int) =
    (for {
      i <- arr.indices
      j <- arr(i).indices
      if arr(i)(j) == '^'
    } yield (i, j)).head

  @tailrec
  private def walk(i: Int, j: Int, dI: Int, dJ: Int, arr: Vector[Vector[Char]], walked: Set[(Int, Int, Int, Int)]): Long = {
    val isBorder = i == 0 || j == 0 || i == arr.length - 1 || j == arr(i).length - 1
    val (nextI, nextJ) = (i + dI, j + dJ)
    val (nextDI, nextDJ) = nextDirection(dI, dJ)
    if (isBorder) 0
    else if (arr(nextI)(nextJ) == '#') walk(i, j, nextDI, nextDJ, arr, walked)
    else if (walked(i, j, dI, dJ)) 1
    else walk(nextI, nextJ, dI, dJ, arr, walked + ((i, j, dI, dJ)))
  }

  private def nextDirection(dI: Int, dJ: Int): (Int, Int) =
    (dI, dJ) match {
      case (1, 0) => (0, -1)
      case (0, -1) => (-1, 0)
      case (-1, 0) => (0, 1)
      case (0, 1) => (1 ,0)
    }
}
