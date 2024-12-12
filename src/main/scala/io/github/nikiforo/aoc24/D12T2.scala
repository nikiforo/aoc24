package io.github.nikiforo.aoc24

object D12T2 {

  private implicit class arrayArrayOps[V](val arr: Array[Array[V]]) extends AnyVal {

    def apply(c: Coord): V = arr(c.i)(c.j)
    def indices2d: List[Coord] = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _))).toList
    def inBorder(c: Coord): Boolean = 0 <= c.i && c.i < arr.length && 0 <= c.j && c.j < arr(c.i).length
  }

  private case class Coord(i: Int, j: Int) {

    def up: Coord = Coord(i + 1, j)
    def down: Coord = Coord(i - 1, j)
    def left: Coord = Coord(i, j - 1)
    def right: Coord = Coord(i, j + 1)
    def neighbour4: List[Coord] = List(up, down, left, right)
  }

  def main(args: Array[String]): Unit = {
    val lines = aocLines("12")
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    listRegions(arr).map(region => countSides(region) * region.size).sum
  }

  private def listRegions(arr: Array[Array[Char]]): List[List[Coord]] =
    arr.indices2d.foldLeft((Set.empty[Coord], List.empty[List[Coord]])) { case ((visited, regions), coord) =>
      if (visited(coord)) (visited, regions)
      else {
        val region = getRegion(arr, arr(coord), List(coord), Set.empty)
        (visited ++ region, region :: regions)
      }
    }._2

  def countSides(region: List[Coord]): Long = {
    val set = region.toSet
    val top = countClusters(region.filter(c => !set(c.up)))
    val bottom = countClusters(region.filter(c => !set(c.down)))
    val right = countClusters(region.filter(c => !set(c.right)))
    val left = countClusters(region.filter(c => !set(c.left)))
    top + bottom + right + left
  }

  private def getRegion(arr: Array[Array[Char]], c: Char, visit: List[Coord], visited: Set[Coord]): List[Coord] =
    visit match {
      case Nil => Nil
      case h :: tail =>
        if (visited(h)) getRegion(arr, c, tail, visited)
        else if (arr(h) == c) h :: getRegion(arr, c, h.neighbour4.filter(arr.inBorder) ::: tail, visited + h)
        else getRegion(arr, c, tail, visited)
    }

  // group coordinates into clusters that are neighbours
  private def countClusters(list: List[Coord]): Int =
    list.foldLeft(List.empty[Set[Coord]]) { (sets, c) =>
      val (cluster, others) = sets.partition(set => c.neighbour4.exists(set))
      (Set(c) :: cluster).reduce(_ ++ _) :: others
    }.size
}
