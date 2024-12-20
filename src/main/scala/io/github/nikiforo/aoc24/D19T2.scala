package io.github.nikiforo.aoc24

import scala.collection.mutable

object D19T2 {

  private abstract class TopDownTab[S, O](tab: mutable.Map[S, O]) {

    final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

    protected def bare(s: S): O
  }

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("19")
    println(aocCompute(lines))
  }

  def aocCompute(lines: List[String]): Long = {
    val (availables, patterns) = parse(lines)
    val tabbed = new TopDownTab(mutable.Map.empty[String, Long]) {
      def bare(pattern: String): Long =
        if (pattern.isEmpty) 1
        else
          availables.map { av =>
            if (pattern.startsWith(av)) compute(pattern.drop(av.length))
            else 0
          }.sum
    }

    patterns.map(tabbed.compute).sum
  }

  def parse(lines: List[String]): (Array[String], List[String]) =
    (lines.head.split(", "), lines.drop(2))
}
