package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D16T1Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """###############
        |#.......#....E#
        |#.#.###.#.###.#
        |#.....#.#...#.#
        |#.###.#####.#.#
        |#.#.#.......#.#
        |#.#.#####.###.#
        |#...........#.#
        |###.#.#####.#.#
        |#...#.....#.#.#
        |#.#.#.###.#.#.#
        |#.....#...#.#.#
        |#.###.#.#.#.#.#
        |#S..#.....#...#
        |###############""".stripMargin.linesIterator.toList

    assert(D16T1.compute(lines) == 7036)
  }
}
