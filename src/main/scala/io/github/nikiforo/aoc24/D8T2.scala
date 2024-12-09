package io.github.nikiforo.aoc24

object D8T2 {

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

  def computeSet(arr: Array[Array[Char]]): Set[(Int, Int)] = {
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
        is = (-100 to 100).map(k => i1 + k * (i2 - i1))
        js = (-100 to 100).map(k => j1 + k * (j2 - j1))
        coord <- is.zip(js)
      } yield coord
    }.toSet
  }
}
