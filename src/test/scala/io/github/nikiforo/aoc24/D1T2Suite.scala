package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D1T2Suite extends AnyFunSuite {

  test("example") {
    val text =
      """3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3""".stripMargin.linesIterator.toList

    println(D1T2.compute(text))
  }
}
