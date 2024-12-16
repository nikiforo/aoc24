package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D15T1Suite extends AnyFunSuite {

  test("example 1") {
    val lines =
      """##########
        |#..O..O.O#
        |#......O.#
        |#.OO..O.O#
        |#..O@..O.#
        |#O#..O...#
        |#O..O..O.#
        |#.OO.O.OO#
        |#....O...#
        |##########
        |
        |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin.linesIterator.toList

    assert(D15T1.compute(lines) == 10092)
  }

  test("gps") {
    val lines =
      """##########
        |#.O.O.OOO#
        |#........#
        |#OO......#
        |#OO@.....#
        |#O#.....O#
        |#O.....OO#
        |#O.....OO#
        |#OO....OO#
        |##########""".stripMargin.linesIterator.toList

    assert(D15T1.gps(lines.toVector.map(_.toVector)) == 10092)
  }
}
