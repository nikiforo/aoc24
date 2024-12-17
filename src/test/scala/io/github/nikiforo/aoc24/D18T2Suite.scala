package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D18T2Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """5,4
        |4,2
        |4,5
        |3,0
        |2,1
        |6,3
        |2,4
        |1,5
        |0,6
        |3,3
        |2,6
        |5,1
        |1,2
        |5,5
        |2,5
        |6,5
        |1,4
        |0,4
        |6,4
        |1,1
        |6,1
        |1,0
        |0,5
        |1,6
        |2,0""".stripMargin.linesIterator.toList

    assert(D18T2.compute(lines, 6, 6, 12) == "Coord(6,1)")
  }
}
