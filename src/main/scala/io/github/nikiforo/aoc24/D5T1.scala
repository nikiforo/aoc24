package io.github.nikiforo.aoc24

object D5T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("5")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val rules = lines.takeWhile(_ != "").toSet
    val pageList = lines.dropWhile(_ != "").tail

    val parsedRules = rules.map { case s"$one|$other" => (one.toLong, other.toLong) }
    pageList.map { pages =>
      val splitted = pages.split(',').map(_.toLong)
      val correct =
        for {
          i <- splitted.indices
          j <- i + 1 until splitted.length
        } yield !parsedRules((splitted(j), splitted(i)))
      if (correct.forall(identity)) splitted(splitted.length / 2) else 0
    }.sum
  }
}
