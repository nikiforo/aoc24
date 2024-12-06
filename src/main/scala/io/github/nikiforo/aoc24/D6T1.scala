package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D6T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("6")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    val (one, other) = initial(arr)
    walk(one, other, -1, 0, arr, Set((one, other)))
  }

  private def initial(arr: Array[Array[Char]]): (Int, Int) =
    (for {
      i <- arr.indices
      j <- arr(i).indices
      if arr(i)(j) == '^'
    } yield (i, j)).head

  @tailrec
  private def walk(i: Int, j: Int, deltaI: Int, deltaJ: Int, arr: Array[Array[Char]], walked: Set[(Int, Int)]): Long = {
    val isBorder = i == 0 || j == 0 || i == arr.length - 1 || j == arr(i).length - 1
    val (nextI, nextJ) = (i + deltaI, j + deltaJ)
    val (nextDeltaI, nextDeltaJ) = nextDirection(deltaI, deltaJ)
    if (isBorder) walked.size
    else if (arr(nextI)(nextJ) == '#') walk(i, j, nextDeltaI, nextDeltaJ, arr, walked)
    else walk(nextI, nextJ, deltaI, deltaJ, arr, walked + ((nextI, nextJ)))
  }

  private def nextDirection(i: Int, j: Int): (Int, Int) =
    (i, j) match {
      case (1, 0) => (0, -1)
      case (0, -1) => (-1, 0)
      case (-1, 0) => (0, 1)
      case (0, 1) => (1 ,0)
    }
}
