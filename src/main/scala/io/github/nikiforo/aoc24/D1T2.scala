package io.github.nikiforo.aoc24

object D1T2 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("1")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val (ones, others) = lines.map { case s"$i   $j" => (i.toInt, j.toInt) }.unzip
    val map = others.groupBy(identity).map { case (i, n) => i -> n.size }
    ones.map(i => i * map.getOrElse(i, 0)).sum
  }
}
