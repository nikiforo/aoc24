package io.github.nikiforo.aoc24

object D2T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("2")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    lines.count { line =>
      val list = line.split(" ").toList
      val diffs = list.zip(list.tail).map { case (i, j) => i.toInt - j.toInt }
      diffs.forall(i => 0 < i && i <= 3) || diffs.forall(i => -3 <= i && i < 0)
    }
}
