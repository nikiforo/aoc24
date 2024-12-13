package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D13T1Suite extends AnyFunSuite {

  test("simple") {
    val lines =
      """Button A: X+94, Y+34
        |Button B: X+22, Y+67
        |Prize: X=116, Y=101""".stripMargin.linesIterator.toList

    assert(D13T1.compute(lines) == 4)
  }

  test("280") {
    val lines =
      """Button A: X+94, Y+34
        |Button B: X+22, Y+67
        |Prize: X=8400, Y=5400""".stripMargin.linesIterator.toList
    assert(D13T1.compute(lines) == 280)
  }

  test("200") {
    val lines =
      """Button A: X+17, Y+86
        |Button B: X+84, Y+37
        |Prize: X=7870, Y=6450""".stripMargin.linesIterator.toList

    assert(D13T1.compute(lines) == 200)
  }

  test("example") {
    val lines = """Button A: X+94, Y+34
                  |Button B: X+22, Y+67
                  |Prize: X=8400, Y=5400
                  |
                  |Button A: X+26, Y+66
                  |Button B: X+67, Y+21
                  |Prize: X=12748, Y=12176
                  |
                  |Button A: X+17, Y+86
                  |Button B: X+84, Y+37
                  |Prize: X=7870, Y=6450
                  |
                  |Button A: X+69, Y+23
                  |Button B: X+27, Y+71
                  |Prize: X=18641, Y=10279""".stripMargin.linesIterator.toList

    assert(D13T1.compute(lines) == 480)
  }
}
