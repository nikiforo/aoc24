package io.github.nikiforo.aoc24

import scala.collection.mutable

object D12T2 {

  def main(args: Array[String]): Unit = {
    val lines = aocLines("12")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    listRegions(arr).map(region => countSides(region) * region.size).sum
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

  def countSides(region: List[(Int, Int)]): Long = {
    val set = region.toSet
    val top = countClusters(region.filter { case (i, j) => !set((i + 1, j)) })
    val bottom = countClusters(region.filter { case (i, j) => !set((i - 1, j)) })
    val right = countClusters(region.filter { case (i, j) => !set((i, j + 1)) })
    val left = countClusters(region.filter { case (i, j) => !set((i, j - 1)) })
    top + bottom + right + left
  }

  // group coordinates into clusters that are neighbours
  private def countClusters(list: List[(Int, Int)]): Int =
    list.foldLeft(List.empty[Set[(Int, Int)]]) { case (sets, (i, j)) =>
      val (cluster, others) = sets.partition(set => neighbour4(i, j).exists(set))
      (Set((i, j)) :: cluster).reduce(_ ++ _) :: others
    }.size

  private def neighbour4(i: Int, j: Int): List[(Int, Int)] = List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
}
