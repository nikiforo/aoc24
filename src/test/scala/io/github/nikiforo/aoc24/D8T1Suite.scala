package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D8T1Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """..........
        |..........
        |..........
        |....a.....
        |..........
        |.....a....
        |..........
        |..........
        |..........
        |..........""".stripMargin.linesIterator.toList

    assert(D8T1.compute(lines) == 2)
  }

  test("example 2") {
    val lines =
      """..........
        |..........
        |..........
        |....a.....
        |........a.
        |.....a....
        |..........
        |..........
        |..........
        |..........""".stripMargin.linesIterator.toList

    assert(D8T1.compute(lines) == 4)
  }

  test("example3") {
    val lines =
      """......#....#
        |...#....0...
        |....#0....#.
        |..#....0....
        |....0....#..
        |.#....A.....
        |...#........
        |#......#....
        |........A...
        |.........A..
        |..........#.
        |..........#.""".stripMargin.linesIterator.toList

    assert(D8T1.compute(lines) == 14)
  }

  test("graph 1") {
    val lines =
      """............
        |........0...
        |.....0......
        |............
        |............
        |............
        |............
        |............
        |............
        |............""".stripMargin.linesIterator.toList

    D8T1.printGraph(lines)
  }

  test("graph3") {
    val lines =
      """......#....#
        |...#....0...
        |....#0....#.
        |..#....0....
        |....0....#..
        |.#....A.....
        |...#........
        |#......#....
        |........A...
        |.........A..
        |..........#.
        |..........#.""".stripMargin.linesIterator.toList

    D8T1.printGraph(lines)
  }

  test("graph3-2") {
    val lines =
      """......#....#
        |...#....0...
        |.....0....#.
        |..#....0....
        |....0....#..""".stripMargin.linesIterator.toList

    D8T1.printGraph(lines)
  }
}
