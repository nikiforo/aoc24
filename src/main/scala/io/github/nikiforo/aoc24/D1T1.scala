package io.github.nikiforo.aoc24

object D1T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("1")
    val (one, other) = lines.map { case s"$i   $j" => (i.toInt, j.toInt) }.unzip
    val answer = one.sorted.zip(other.sorted).map { case (i, j) => math.abs(i - j) }.sum
    println(answer)
  }
}
