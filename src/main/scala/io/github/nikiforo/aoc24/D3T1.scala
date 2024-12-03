package io.github.nikiforo.aoc24

object D3T1 {

  private val Regex = """mul\(\d+,\d+\)""".r

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("3")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    lines.flatMap { line =>
      Regex.findAllMatchIn(line).map { regs =>
        regs.matched match {
          case s"mul($i,$j)" => i.toLong * j.toLong
        }
      }
    }.sum
}
