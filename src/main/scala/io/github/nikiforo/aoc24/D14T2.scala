package io.github.nikiforo.aoc24

object D14T2 {

  private case class Robot(x: Long, y: Long, dx: Long, dy: Long)

  private case class Coord(i: Long, j: Long) {
    def neighbour4: List[Coord] = List((i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)).map(p => Coord(p._1, p._2))
  }

  private val height = 103
  private val width = 101

  def main(args: Array[String]): Unit = {
    val lines = aocLines("14")
    val robots = lines.map(parse)
    (0 to Int.MaxValue).foreach { secs =>
      println(secs)
      val positions = simulate(robots, secs)
      val clusters = getClusters(positions.map(p => Coord(p._1, p._2)))
      if (clusters.exists(_.size >= 20)) println(scheme(robots, secs))
    }
  }

  private def simulate(robots: List[Robot], secs: Int): List[(Long, Long)] =
    robots.map { r =>
      val x = ((r.x + r.dx * secs) % width + width) % width
      (x, ((r.y + r.dy * secs) % height + height) % height)
    }

  private def parse(line: String): Robot =
    line match {
      case s"p=$x,$y v=$dx,$dy" => Robot(x.toLong, y.toLong, dx.toLong, dy.toLong)
    }

  private def getClusters(list: List[Coord]): List[Set[Coord]] =
    list.foldLeft(List.empty[Set[Coord]]) { (sets, c) =>
      val (cluster, others) = sets.partition(set => c.neighbour4.exists(set))
      (Set(c) :: cluster).reduce(_ ++ _) :: others
    }

  private def scheme(robots: List[Robot], secs: Int): String = {
    val arr = Array.fill(height)(Array.fill(width)(' '))
    simulate(robots, secs).foreach { case (x, y) =>
      arr(y.toInt)(x.toInt) = '*'
    }
    arr.map(_.mkString("")).mkString("\n")
  }
}
