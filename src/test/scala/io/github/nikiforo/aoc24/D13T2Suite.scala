package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D13T2Suite extends AnyFunSuite {

  test("example 2-2") {
    val lines =
      """Button A: X+26, Y+66
        |Button B: X+67, Y+21
        |Prize: X=12748, Y=12176
        """.stripMargin.linesIterator.toList

    assert(D13T2.compute(lines) == 459236326669L)
  }
}
