package io.github.nikiforo.aoc24

object D5T2 {

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
      def isCorrect =
        (for {
          i <- splitted.indices
          j <- i + 1 until splitted.length
        } yield !parsedRules((splitted(j), splitted(i)))).forall(identity)
      if(isCorrect) 0
      else {
        while (!isCorrect) {
          for {
            i <- splitted.indices
            j <- i + 1 until splitted.length
            if parsedRules((splitted(j), splitted(i)))
          } {
            val right = splitted(j)
            splitted(j) = splitted(i)
            splitted(i) = right
          }
        }

        splitted(splitted.length / 2)
      }
    }.sum
  }
}
