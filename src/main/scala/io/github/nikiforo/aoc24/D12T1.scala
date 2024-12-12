package io.github.nikiforo.aoc24

import scala.collection.mutable

object D12T1 {

  def main(args: Array[String]): Unit = {
    val lines = aocLines("12")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    listRegions(arr).map { region =>
      val set = region.toSet
      set.toList.map { case (i, j) =>
        neighbour4(i, j).count { case (ni, nj) => !set(ni, nj) }
      }.sum * set.size
    }.sum
  }

  private def listRegions(arr: Array[Array[Char]]): Seq[List[(Int, Int)]] = {
    val visited = mutable.Set.empty[(Int, Int)]
    def inBorder(p: (Int, Int)): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < arr.length && p._2 < arr(p._1).length
    def findRegion(arr: Array[Array[Char]], c: Char, i: Int, j: Int): List[(Int, Int)] =
      if (visited((i, j))) List.empty
      else if (arr(i)(j) == c) {
        visited += ((i, j))
        (i, j) :: neighbour4(i, j).filter(inBorder).flatMap { case (ni, nj) => findRegion(arr, c, ni, nj) }
      } else List.empty

    for {
      i <- arr.indices
      j <- arr(i).indices
      if !visited((i, j))
    } yield findRegion(arr, arr(i)(j), i, j)
  }

  private def neighbour4(i: Int, j: Int): List[(Int, Int)] = List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
}
