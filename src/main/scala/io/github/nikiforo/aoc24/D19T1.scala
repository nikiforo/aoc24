package io.github.nikiforo.aoc24

import scala.collection.mutable

object D19T1 {

  private abstract class TopDownTab[S, O](tab: mutable.Map[S, O]) {

    final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

    protected def bare(s: S): O
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("19")
    println(aocCompute(lines))
  }

  def aocCompute(lines: List[String]): Long = {
    val (availables, patterns) = parse(lines)
    val tabbed = new TopDownTab(mutable.Map.empty[String, Boolean]) {
      def bare(pattern: String): Boolean =
        if (pattern.isEmpty) true
        else
          availables.exists { av =>
            pattern.startsWith(av) && compute(pattern.drop(av.length))
          }
    }

    patterns.count(tabbed.compute)
  }

  def parse(lines: List[String]): (Array[String], List[String]) =
    (lines.head.split(", "), lines.drop(2))
}
