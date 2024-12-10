package io.github.nikiforo.aoc24

import scala.annotation.tailrec

object D9T1 {

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("9")
    println(compute(lines))
  }

  def compute(lines: List[String]): BigInt = {
    val vector = lines.head.toList.map(_ - '0').grouped(2).toVector.zipWithIndex.flatMap {
      case (List(filled, empty), i) => List.fill(filled)(Some(i)) ::: List.fill(empty)(Option.empty[Int])
      case (List(filled), i) => List.fill(filled)(Some(i))
    }
    compact(vector, List.empty).zipWithIndex.map(p => p._1.toLong * p._2).sum
  }

  @tailrec
  def compact(vector: Vector[Option[Int]], acc: List[Int]): List[Int] =
    if (vector.isEmpty) acc.reverse
    else
      vector.head match {
        case None => compact(vector.last +: vector.tail.init, acc)
        case Some(i) => compact(vector.tail, i :: acc)
      }
}
