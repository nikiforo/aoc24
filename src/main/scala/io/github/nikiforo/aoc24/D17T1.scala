package io.github.nikiforo.aoc24

object D17T1 {

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

  def compute(lines: List[String]): String = {
    val (registers, program) = parse(lines)
    go(registers, program, 0, List.empty)._2.reverse.mkString(",")
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
    lines match {
      case List(
        s"Register A: $i",
        s"Register B: $j",
        s"Register C: $k",
        "",
        s"Program: $program") => (Registers(i.toLong, j.toInt, k.toInt), program.split(",").map(_.toInt).toVector)
    }
}
