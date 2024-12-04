package io.github.nikiforo.aoc24

import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

@Ignore
final class D4T2Suite extends AnyFunSuite {

  test("small 2") {
    val lines =
      """MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX""".stripMargin.linesIterator.toList


    assert(D4T2.compute(lines) == 9)
  }
}
