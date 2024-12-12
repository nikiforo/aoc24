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


  test("sides") {
    val sides = D12T2.countSides(List((0, 0), (0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (2, 3)))
    assert(sides == 12)
  }
}
