package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D12T2Suite extends AnyFunSuite{

  test("example") {
    val lines =
      """AAAA
        |BBCD
        |BBCC
        |EEEC""".stripMargin.linesIterator.toList

    assert(D12T2.compute(lines) == 80)
  }

  test("example 3") {
    val lines =
      """EEEEE
        |EXXXX
        |EEEEE
        |EXXXX
        |EEEEE""".stripMargin.linesIterator.toList

    assert(D12T2.compute(lines) == 236)
  }

  test("example 4") {
    val lines =
      """AAAAAA
        |AAABBA
        |AAABBA
        |ABBAAA
        |ABBAAA
        |AAAAAA""".stripMargin.linesIterator.toList

    assert(D12T2.compute(lines) == 368)
  }
}
