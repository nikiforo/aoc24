package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D3T1Suite extends AnyFunSuite {
  test("example") {
    println(D3T1.compute(List("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")))
  }
}
