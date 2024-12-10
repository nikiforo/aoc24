package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D10T1Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """10..9..
        |2...8..
        |3...7..
        |4567654
        |...8..3
        |...9..2
        |.....01""".stripMargin.linesIterator.toList

    assert(D10T1.compute(lines) == 3)
  }

  test("example2") {
    val lines =
      """..90..9
        |...1.98
        |...2..7
        |6543456
        |765.987
        |876....
        |987....""".stripMargin.linesIterator.toList

    assert(D10T1.compute(lines) == 4)
  }

  test("example3") {
    val lines =
      """89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732""".stripMargin.linesIterator.toList

    assert(D10T1.compute(lines) == 36)
  }
}
