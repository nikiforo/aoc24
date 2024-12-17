package io.github.nikiforo.aoc24

import scala.collection.parallel.CollectionConverters._

// Not solved
object D17T2 {

  case class Registers(a: Long, b: Long, c: Long)

  case class Combo(registers: Registers) {

    def unapply(h: Int): Option[Long] =
      h match {
        case h if h < 4 => Some(h)
        case 4 => Some(registers.a)
        case 5 => Some(registers.b)
        case 6 => Some(registers.c)
        case _ => None
      }
  }

  def solve(args: Array[String]): Unit = {
    val lines = aocLines("17")
    println(s"\n\n\n BINGO: ${compute(lines)}\n\n\n")
  }

  private val blockSize = 100_000_000L

  def compute(lines: List[String]): Long = { // 35184372088832L 1116s 6678 35852172088832
    val start = 35184372088832L
    val (registers, program) = parse(lines)
    val programList = program.toList
    (6677 until Int.MaxValue).iterator.grouped(4).flatMap { seq =>
      seq.par.flatMap { n =>
        val from = start + blockSize * n
        val to = start + blockSize * (n + 1)
        println(s"${System.currentTimeMillis() / 1000 % 1000 + 1000}s $n $from")
        (from until to).filter { i =>
          go(registers.copy(a = i), program, 0, programList)
        }
      }
    }.next()
  }

  def go(regs: Registers, program: Vector[Int], pointer: Int, data: List[Int]): Boolean = {
    val CMB = Combo(regs)
    if (pointer >= program.length) data.isEmpty
    else {
      (program(pointer), program(pointer + 1)) match {
        case (0, CMB(h)) => go(regs.copy(a = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
        case (1, h) => go(regs.copy(b = regs.b ^ h), program, pointer + 2, data)
        case (2, CMB(h)) => go(regs.copy(b = h % 8), program, pointer + 2, data)
        case (3, h) => if (regs.a == 0) go(regs, program, pointer + 2, data) else go(regs, program, h, data)
        case (4, _) => go(regs.copy(b = regs.b ^ regs.c), program, pointer + 2, data)
        case (5, CMB(h)) =>
          data.headOption match {
            case None => false
            case Some(d) => if (d == h % 8) go(regs, program, pointer + 2, data.tail) else false
          }
        case (6, CMB(h)) => go(regs.copy(b = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
        case (7, CMB(h)) => go(regs.copy(c = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
      }
    }
  }

  private def parse(lines: List[String]) =
    lines match {
      case List(s"Register A: $i", s"Register B: $j", s"Register C: $k", "", s"Program: $program") =>
        (Registers(i.toInt, j.toInt, k.toInt), program.split(",").map(_.toInt).toVector)
    }
}
