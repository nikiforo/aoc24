package io.github.nikiforo.aoc24

object D3T2 {

  private val Regex = """mul\(\d+,\d+\)|don't\(\)|do\(\)""".r

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("3")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    doCompute(lines.flatMap(Regex.findAllMatchIn(_).map(_.matched)), active = true)

  def doCompute(tokens: List[String], active: Boolean): Long = tokens match {
    case Nil => 0
    case s"mul($i,$j)" :: tail => (if (active) i.toLong * j.toLong else 0) + doCompute(tail, active)
    case "don't()" :: tail => doCompute(tail, active = false)
    case "do()" :: tail => doCompute(tail, active = true)
  }
}
