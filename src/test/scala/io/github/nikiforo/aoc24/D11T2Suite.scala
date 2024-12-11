package io.github.nikiforo.aoc24

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

@DoNotDiscover
final class D11T2Suite extends AnyFunSuite {

  test("example 1") {
    val lines = """125 17""".stripMargin.linesIterator.toList
    println(D11T2.aocCompute(lines, 6))
  }
}
