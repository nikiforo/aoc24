package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D3T2Suite extends AnyFunSuite {
  test("example") {
    println(D3T2.compute(List("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")))
  }
}
