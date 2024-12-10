package io.github.nikiforo.aoc24

object D10T2 {

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
    } yield go(arr, i, j)).sum
  }

  private def go(arr: Array[Array[Int]], i: Int, j: Int): Long = {
    def inBorder(p: (Int, Int)): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < arr.length && p._2 < arr(p._1).length
    val c = arr(i)(j)
    if (c == 9) 1
    else neighbour4(i, j).filter(inBorder).filter(p => arr(p._1)(p._2) == c + 1).map(p => go(arr, p._1, p._2)).sum
  }

  private def neighbour4(i: Int, j: Int): List[(Int, Int)] = List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
}
