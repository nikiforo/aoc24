package io.github.nikiforo.aoc24

import scala.collection.mutable

object D13T1 {

  private abstract class TopDownTab[S, O](tab: mutable.Map[S, O]) {

    final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

    protected def bare(s: S): O
  }

  private case class Coord(x: Int, y: Int) {
    def minus(other: Coord): Coord = Coord(x - other.x, y - other.y)
    def isNegative: Boolean = x < 0 && y < 0
  }

  private case class State(remaining: Coord, cost: Int, a1: Int, a2: Int)

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("13")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    parse(lines).map { case (x1, y1, x2, y2, xR, yR) =>
      val c1 = Coord(x1, y1)
      val c2 = Coord(x2, y2)
      val tabbed = new TopDownTab(mutable.Map.empty[State, Int]) {
        protected def bare(s: State): Int = {
          val State(remaining: Coord, cost: Int, a1: Int, a2: Int) = s
          if (remaining.isNegative || a1 > 100 || a2 > 100) Int.MaxValue
          else if (remaining == Coord(0, 0)) cost
          else List(
            compute(State(remaining.minus(c1), cost + 3, a1 + 1, a2)),
            compute(State(remaining.minus(c2), cost + 1, a1, a2 + 1)),
          ).min
        }
      }

      val x = tabbed.compute(State(Coord(xR, yR), 0, 0, 0))
      if (x == Int.MaxValue) 0 else x
    }.sum

  private def parse(lines: List[String]): List[(Int, Int, Int, Int, Int, Int)] =
    lines match {
      case Nil => Nil
      case s"Button A: X+$i1, Y+$i2" ::
           s"Button B: X+$i3, Y+$i4" ::
           s"Prize: X=$i5, Y=$i6" :: tail => (i1.toInt, i2.toInt, i3.toInt, i4.toInt, i5.toInt, i6.toInt) :: parse(tail.drop(1))
    }
}
