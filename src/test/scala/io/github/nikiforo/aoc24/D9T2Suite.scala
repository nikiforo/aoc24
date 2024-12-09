package io.github.nikiforo.aoc24

import org.scalatest.funsuite.AnyFunSuite

final class D9T2Suite extends AnyFunSuite {

  test("example") {
    assert(D9T2.compute(List("2333133121414131402")) == 2858)
  }
}
