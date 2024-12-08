package io.github.nikiforo.aoc24

object D7T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("7")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    lines.map {
      case s"$testStr: $params" =>
        val test = testStr.toLong
        if (validate(test, params.split(' ').map(_.toLong).toList, 0)) test
        else 0
    }.sum

  private def validate(test: Long, params: List[Long], acc: Long): Boolean =
    acc <= test && (params match {
      case h :: tail => validate(test, tail, acc + h) || validate(test, tail, acc * h)
      case Nil => acc == test
    })
}
