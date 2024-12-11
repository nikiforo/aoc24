package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D11T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("11")
    println(compute(lines))
  }

  private def compute(lines: List[String]): Long = {
    blink(lines.head.split(' ').toVector, 25).length
  }

  @tailrec
  private def blink(stones: Vector[String], num: Int): Vector[String] =
    if (num <= 0) stones
    else blink(stones.flatMap(rule), num - 1)

  private def rule(stone: String): Vector[String] =
    if (stone == "0") Vector("1")
    else if (stone.length % 2 == 0) {
      val half = stone.length / 2
      val left = stone.take(half).dropWhile(_ == '0')
      val right = stone.drop(stone.length / 2).dropWhile(_ == '0')
      Vector(left, if(right.isEmpty) "0" else right)
    } else Vector((stone.toLong * 2024).toString)
}
