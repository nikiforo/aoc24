package io.github.nikiforo.aoc24

object D7T2 {

  def main(args: Array[String]): Unit = {
    val lines = aocLines("7")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long =
    lines.map(parsed).sum

  def parsed(line: String): Long = line match {
    case s"$testStr: $params" =>
      val test = testStr.toLong
      if (validate(test, params.split(' ').map(_.toLong).toList)) test
      else 0
  }

  def validate(test: Long, params: List[Long]): Boolean =
    params match {
      case h :: Nil => h == test
      case h1 :: h2 :: tail =>
        h1 <= test &&
          validate(test, h1 * h2 :: tail) ||
          validate(test, h1 + h2 :: tail) ||
          validate(test, s"$h1$h2".toLong :: tail )
      case Nil => false
    }
}
