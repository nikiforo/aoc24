package io.github.nikiforo.aoc24

import io.github.nikiforo.aoc24.D17T1.Registers
import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D17T1Suite extends AnyFunSuite {

  test("example") {
    val lines =
      """Register A: 729
        |Register B: 0
        |Register C: 0
        |
        |Program: 0,1,5,4,3,0
        |""".stripMargin.linesIterator.toList

    assert(D17T1.compute(lines) == "4,6,3,5,6,3,5,2,1,0")
  }

  test("example 1") {
    assert(D17T1.go(Registers(0, 0, 9), Vector(2, 6), 0, List.empty)._1.b == 1)
  }

  test("example 2") {
    assert(D17T1.go(Registers(10, 0, 0), Vector(5, 0, 5, 1, 5, 4), 0, List.empty)._2.reverse == List(0, 1, 2))
  }
}
