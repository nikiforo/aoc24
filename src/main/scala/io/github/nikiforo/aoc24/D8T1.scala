package io.github.nikiforo.aoc24

object D8T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("8")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    def inBound(i: Int, j: Int): Boolean = 0 <= i && i < arr.length && 0 <= j && j < arr(i).length
    val set = computeSet(arr)
    set.count { case (i, j) => inBound(i, j) }
  }

  private def computeSet(arr: Array[Array[Char]]): Set[(Int, Int)] = {
    val antennas =
      for {
        i <- arr.indices
        j <- arr(i).indices
        if arr(i)(j) != '.' && arr(i)(j) != '#'
      } yield (i, j, arr(i)(j))

    antennas.groupBy(_._3).toList.flatMap { case (_, grouped) =>
      for {
        i <- grouped.indices
        j <- (i + 1) until grouped.length
        (i1, j1, _) = grouped(i)
        (i2, j2, _) = grouped(j)
        antiI1 = i2 + (i2 - i1)
        antiI2 = i1 - (i2 - i1)
        antiJ1 = j2 + (j2 - j1)
        antiJ2 = j1 - (j2 - j1)
        coord <- List((antiI1, antiJ1), (antiI2, antiJ2))
      } yield coord
    }.toSet
  }

  def printGraph(lines: List[String]): Unit = {
    val arr = lines.toArray.map(_.toArray)
    def inBound(i: Int, j: Int): Boolean = 0 <= i && i < arr.length && 0 <= j && j < arr(i).length
    val set = computeSet(arr)
    set.foreach { case (i, j) => if (inBound(i, j)) arr(i)(j) = '@' }
    arr.foreach(arr => println(arr.mkString("")))
  }
}
