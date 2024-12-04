package io.github.nikiforo.aoc24

object D4T2 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("4")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val chars = lines.map(_.toArray).toArray

    def inBorder(p: (Int, Int)) = p._1 >= 0 && p._2 >= 0 && p._1 < chars.length && p._2 < chars(p._1).length

    def applied(p: (Int, Int)) = chars(p._1)(p._2)

    val ks = 0 until 3

    val xmass =
      for {
        i <- chars.indices
        j <- chars.indices
      } yield {
        List(
          ks.map(k => (i + k, j + k)) ++ ks.map(k => (i + 2 - k, j + k)),
          ks.map(k => (i + k, j + k)) ++ ks.map(k => (i + k, j + 2 - k)),
          ks.map(k => (i - k, j + k)) ++ ks.map(k => (i - k, j + 2 - k)),
          ks.map(k => (i + 2 - k, j + 2 - k)) ++ ks.map(k => (i + k, j + 2 - k)),
        ).map(_.filter(inBorder).map(applied)).count(_ == Vector('M', 'A', 'S', 'M', 'A', 'S'))
      }

    xmass.sum
  }
}
