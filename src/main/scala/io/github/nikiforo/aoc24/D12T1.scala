package io.github.nikiforo.aoc24

object D12T1 {

  private case class Coord(i: Int, j: Int) {

    def up: Coord = Coord(i + 1, j)
    def down: Coord = Coord(i - 1, j)
    def left: Coord = Coord(i, j - 1)
    def right: Coord = Coord(i, j + 1)
    def inBorder[V](arr: Array[Array[V]]): Boolean = 0 <= i && i < arr.length && 0 <= j && j < arr(i).length
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("12")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    listRegions(arr).map { region =>
      val set = region.toSet
      region.map(c => neighbour4(c).count(n => !set(n))).sum * region.size
    }.sum
  }

  private def listRegions(arr: Array[Array[Char]]): List[List[Coord]] = {
    val inds = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _)))
    inds.foldLeft((Set.empty[Coord], List.empty[List[Coord]])) { case ((visited, regions), coord) =>
      if (visited(coord)) (visited, regions)
      else {
        val region = go(arr, arr(coord.i)(coord.j), List(coord), Set.empty)
        (visited ++ region, region :: regions)
      }
    }._2
  }

  private def go(arr: Array[Array[Char]], c: Char, visit: List[Coord], visited: Set[Coord]): List[Coord] =
    visit match {
      case Nil => Nil
      case h :: tail =>
        if (visited(h)) go(arr, c, tail, visited)
        else if (arr(h.i)(h.j) == c) h :: go(arr, c, neighbour4(h).filter(_.inBorder(arr)) ::: tail, visited + h)
        else go(arr, c, tail, visited)
    }

  private def neighbour4(c: Coord): List[Coord] = List(c.up, c.down, c.left, c.right)
}
