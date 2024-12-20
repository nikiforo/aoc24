package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D20T1Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """###############
        |#...#...#.....#
        |#.#.#.#.#.###.#
        |#S#...#.#.#...#
        |#######.#.#.###
        |#######.#.#...#
        |#######.#.###.#
        |###..E#...#...#
        |###.#######.###
        |#...###...#...#
        |#.#####.#.###.#
        |#.#...#.#.#...#
        |#.#.#.#.#.#.###
        |#...#...#...###
        |###############""".stripMargin.linesIterator.toList

    assert(D20T1.compute(lines, 1) == 44)
    assert(D20T1.compute(lines, 40) == 2)
    assert(D20T1.compute(lines, 41) == 1)
  }
}
