package io.github.nikiforo.aoc24

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object D13T2 {

  private case class Coord(x: Long, y: Long) {

    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def *(k: Long): Coord = Coord(x * k, y * k)
  }

  val big = 10000000000000L

  def main(args: Array[String]): Unit = {
    val lines = aocLines("13")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    parse(lines).toVector.par.map { case (x1, y1, x2, y2, xR, yR) =>
      val c1 = Coord(x1, y1)
      val c2 = Coord(x2, y2)
      val finish = Coord(big + xR, big + yR)
      go(c2, math.min(finish.x / c2.x, finish.y / c2.y) + 1, c1, 0, finish)
    }.sum

  @tailrec
  def go(c2: Coord, n2: Long, c1: Coord, n1: Long, finish: Coord): Long = {
    val c = c2 * n2 + c1 * n1
    if (n2 < 0) 0
    else if (c == finish) n2 + n1 * 3
    else if (c.x >= finish.x || c.y >= finish.y) go(c2, n2 - 1, c1, n1, finish)
    else go(c2, n2, c1, n1 + 1, finish)
  }

  private def parse(lines: List[String]): List[(Int, Int, Int, Int, Int, Int)] =
    lines match {
      case Nil => Nil
      // format: off
      case s"Button A: X+$i1, Y+$i2" ::
           s"Button B: X+$i3, Y+$i4" ::
           s"Prize: X=$i5, Y=$i6" :: tail => (i1.toInt, i2.toInt, i3.toInt, i4.toInt, i5.toInt, i6.toInt) :: parse(tail.drop(1))
    }
}
