package io.github.nikiforo.aoc24

object D2T2 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("2")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    lines.count { line =>
      val ints = line.split(" ").toList.map(i => i.toInt)
      ints.indices.exists { ind =>
        val dropped = ints.take(ind) ::: ints.drop(ind + 1)
        val diffs = dropped.zip(dropped.tail).map { case (i, j) => i - j }
        diffs.forall(i => 0 < i && i <= 3) || diffs.forall(i => -3 <= i && i < 0)
      }
    }
}
