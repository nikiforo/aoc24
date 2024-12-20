package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D19T1Suite extends AnyFunSuite {

  test("example") {
    val lines = """r, wr, b, g, bwu, rb, gb, br
                  |
                  |brwrr
                  |bggr
                  |gbbr
                  |rrbgbr
                  |ubwu
                  |bwurrg
                  |brgr
                  |bbrgwb""".stripMargin.linesIterator.toList
    assert(D19T1.aocCompute(lines) == 6)
  }
}
