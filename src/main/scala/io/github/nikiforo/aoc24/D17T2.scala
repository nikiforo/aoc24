package io.github.nikiforo.aoc24

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
    println(compute(lines))
  }

  def compute(lines: List[String]): Long = {
    val program = parse(lines)
    program.tails.toList.reverse.tail.foldLeft(List(0L)) { (possible, prog) =>
      possible.flatMap { poss =>
        (0 to 7).map(poss * 8 + _).filter { i =>
          go(Registers(i, 0, 0), program, 0, List.empty)._2.reverse == prog
        }
      }
    }.head
  }

  def go(regs: Registers, program: Vector[Int], pointer: Int, data: List[Long]): (Registers, List[Long]) = {
    val CMB = Combo(regs)
    if (pointer >= program.length) (regs, data)
    else {
      (program(pointer), program(pointer + 1)) match {
        case (0, CMB(h)) => go(regs.copy(a = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
        case (1, h) => go(regs.copy(b = regs.b ^ h), program, pointer + 2, data)
        case (2, CMB(h)) => go(regs.copy(b = h % 8), program, pointer + 2, data)
        case (3, h) => if (regs.a == 0) go(regs, program, pointer + 2, data) else go(regs, program, h, data)
        case (4, _) => go(regs.copy(b = regs.b ^ regs.c), program, pointer + 2, data)
        case (5, CMB(h)) => go(regs, program, pointer + 2, h % 8 :: data)
        case (6, CMB(h)) => go(regs.copy(b = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
        case (7, CMB(h)) => go(regs.copy(c = regs.a / math.pow(2, h).toInt), program, pointer + 2, data)
      }
    }
  }

  private def parse(lines: List[String]) =
    lines.drop(4) match {
      case List(s"Program: $program") => program.split(",").map(_.toInt).toVector
    }
}
