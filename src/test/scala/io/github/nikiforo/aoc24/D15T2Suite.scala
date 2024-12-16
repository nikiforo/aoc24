package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D15T2Suite extends AnyFunSuite {

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
