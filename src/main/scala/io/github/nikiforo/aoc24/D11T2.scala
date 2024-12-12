package io.github.nikiforo.aoc24

import scala.collection.mutable

object D11T2 {

  private abstract class TopDownTab[S, O](tab: mutable.Map[S, O]) {

    final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

    protected def bare(s: S): O
  }

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("11")
    println(aocCompute(lines, 75))
  }

  def aocCompute(lines: List[String], blinks: Int): Long = {
    val topDown = new TopDownTab(mutable.Map.empty[(String, Int), Long]) {
      final def bare(s: (String, Int)): Long = {
        val (stone, num) = s
        if (num <= 0) 1 else rule(stone).map(compute(_, num - 1)).sum
      }
    }
    lines.head.split(' ').map(s => topDown.compute((s, blinks))).sum
  }

  private def rule(stone: String): Vector[String] =
    if (stone == "0") Vector("1")
    else if (stone.length % 2 == 0) {
      val half = stone.length / 2
      val left = stone.take(half).dropWhile(_ == '0')
      val right = stone.drop(half).dropWhile(_ == '0')
      Vector(left, if (right.isEmpty) "0" else right)
    } else Vector((stone.toLong * 2024).toString)
}
