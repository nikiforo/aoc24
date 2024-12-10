package io.github.nikiforo.aoc24

object D10T1 {

  def main(args: Array[String]): Unit = {
    val lines = aocLines("10")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray.map(_ - '0'))
    (for {
      i <- arr.indices
      j <- arr(0).indices
      if arr(i)(j) == 0
      (k, l) <- go(arr, i, j)
    } yield (i, j, k, l)).toSet.size
  }

  private def go(arr: Array[Array[Int]], i: Int, j: Int): List[(Int, Int)] = {
    def inBorder(p: (Int, Int)): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < arr.length && p._2 < arr(p._1).length
    val c = arr(i)(j)
    if (c == 9) List((i, j))
    else neighbour4(i, j).filter(inBorder).filter(p => arr(p._1)(p._2) == c + 1).flatMap(p => go(arr, p._1, p._2))
  }

  private def neighbour4(i: Int, j: Int): List[(Int, Int)] = List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
}
