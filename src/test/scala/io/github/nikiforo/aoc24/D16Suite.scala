package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D16Suite extends AnyFunSuite {

//  test("example") {
//    val lines =
//      """###############
//        |#.......#....E#
//        |#.#.###.#.###.#
//        |#.....#.#...#.#
//        |#.###.#####.#.#
//        |#.#.#.......#.#
//        |#.#.#####.###.#
//        |#...........#.#
//        |###.#.#####.#.#
//        |#...#.....#.#.#
//        |#.#.#.###.#.#.#
//        |#.....#...#.#.#
//        |#.###.#.#.#.#.#
//        |#S..#.....#...#
//        |###############""".stripMargin.linesIterator.toList
//
//    assert(D16T1.compute(lines) == 7036)
//  }
//
//  test("example 2") {
//    val lines =
//      """###############
//        |#.......#....E#
//        |#.#.###.#.###O#
//        |#.....#.#...#O#
//        |#.###.#####.#O#
//        |#.#.#.......#O#
//        |#.#.#####.###O#
//        |#..OOOOOOOOO#O#
//        |###O#O#####O#O#
//        |#OOO#O....#O#O#
//        |#O#O#O###.#O#O#
//        |#OOOOO#...#O#O#
//        |#O###.#.#.#O#O#
//        |#S..#.....#OOO#
//        |###############""".stripMargin.linesIterator.toList
//
//    assert(D16T2.compute(lines) == 45)
//  }
//
  test("example 3") {
    val lines =
      """#################
        |#...#...#...#..E#
        |#.#.#.#.#.#.#.#O#
        |#.#.#.#...#...#O#
        |#.#.#.#.###.#.#O#
        |#OOO#.#.#.....#O#
        |#O#O#.#.#.#####O#
        |#O#O..#.#.#OOOOO#
        |#O#O#####.#O###O#
        |#O#O#..OOOOO#OOO#
        |#O#O###O#####O###
        |#O#O#OOO#..OOO#.#
        |#O#O#O#####O###.#
        |#O#O#OOOOOOO..#.#
        |#O#O#O#########.#
        |#S#OOO..........#
        |#################""".stripMargin.linesIterator.toList

    assert(D16T2.compute(lines) == 64)
  }

  test("prod") {
    val lines = aocLines("16")
    assert(D16T2.compute(lines) == 442)
  }
}
