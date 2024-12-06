package io.github.nikiforo.aoc24

import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

@Ignore
final class D6T2Suite extends AnyFunSuite {

  test("example") {
    val arr =
      """....#.....
        |.........#
        |..........
        |..#.......
        |.......#..
        |..........
        |.#..^.....
        |........#.
        |#.........
        |......#...""".stripMargin.linesIterator.toList

    assert(D6T2.compute(arr) == 6)
  }
}
