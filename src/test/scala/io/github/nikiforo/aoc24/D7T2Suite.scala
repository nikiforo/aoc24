package io.github.nikiforo.aoc24

import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

@Ignore
final class D7T2Suite extends AnyFunSuite {

  test("example 2") {
    val lines =
      """190: 10 19
        |3267: 81 40 27
        |83: 17 5
        |156: 15 6
        |7290: 6 8 6 15
        |161011: 16 10 13
        |192: 17 8 14
        |21037: 9 7 18 13
        |292: 11 6 16 20
        |""".stripMargin.linesIterator.toList

    assert(D7T2.compute(lines) == 11387)
  }

  test("7290") {
    assert(D7T2.parsed("7290: 6 8 6 15") == 7290)
  }
}
