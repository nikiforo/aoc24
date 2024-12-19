package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D19T2Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """r, wr, b, g, bwu, rb, gb, br
        |
        |brwrr
        |bggr
        |gbbr
        |rrbgbr
        |ubwu
        |bwurrg
        |brgr
        |bbrgwb""".stripMargin.linesIterator.toList
    assert(D19T2.aocCompute(lines) == 16)
  }
}
