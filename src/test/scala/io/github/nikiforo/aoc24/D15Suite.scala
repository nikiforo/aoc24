package io.github.nikiforo.aoc24

import io.github.nikiforo.aoc24.D15T2._
import org.scalatest.funsuite.AnyFunSuite

final class D15Suite extends AnyFunSuite {

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

  test("gps2") {
    val lines =
      """####################
        |##[].......[].[][]##
        |##[]...........[].##
        |##[]........[][][]##
        |##[]......[]....[]##
        |##..##......[]....##
        |##..[]............##
        |##..@......[].[][]##
        |##......[][]..[]..##
        |####################""".stripMargin.linesIterator.toList

    assert(D15T2.gps(lines.toVector.map(_.toVector)) == 9021)
  }

  test("example 2") {
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

    assert(D15T2.compute(lines) == 9021)
  }
}
