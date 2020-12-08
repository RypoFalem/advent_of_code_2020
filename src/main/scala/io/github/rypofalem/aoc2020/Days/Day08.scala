package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day08 extends Problem {
  override def uid: Int = 8
  private val linePattern = """(\w+) ([+|-]\d+)""".r
  val code: Vector[Op] = lines.toVector map  {
    case linePattern(code, n) => code match {
      case "nop" => Nop(n.toInt)
      case "acc" => Acc(n.toInt)
      case "jmp" => Jmp(n.toInt)
    }
  }

  class Machine(val codes: Vector[Op]){
    var accum: Int = 0
    var cursor: Int = 0
    var terminatedGracefully: Boolean = false
    private var positions: Set[Int] = Set()

    // execute code at current cursor
    def execute(): Unit = {
      positions += cursor
      codes(cursor).execute(this)

    }

    // execute until infinite loop is detected
    def run(): Unit = {
      while (stillRunning) execute()
    }

    def stillRunning: Boolean = {
      if(cursor == codes.length) {
        terminatedGracefully = true
        false
      } else if(cursor < 0 || cursor > codes.length) false
      else !positions.contains(cursor)
    }
  }

  sealed trait Op {
    val instr: Int
    val code: String
    def execute(mach: Machine): Unit
  }
  case class Nop(instr: Int) extends Op {
    override val code: String = "nop"
    override def execute(mach: Machine): Unit = {
      mach.cursor += 1
    }
  }
  case class Acc(instr: Int) extends Op{
    override val code: String = "acc"
    override def execute(mach: Machine): Unit = {
      mach.accum += instr
      mach.cursor += 1
    }
  }
  case class Jmp(instr: Int) extends Op {
    override val code: String = "jmp"
    override def execute(mach: Machine): Unit = mach.cursor += instr
  }

  override def solution1: String = {
    val mach = new Machine(code)
    mach.run()
    s"Before a loop was detected, the accum is ${mach.accum}"
  }

  override def solution2: String = {
    val programs = for{
      (c,i) <- code.to(LazyList).zipWithIndex
      if c.code == "jmp" || c.code == "nop"
    } yield {
      if(c.code == "jmp") new Machine(code.updated(i, Nop(c.instr)))
      else                new Machine(code.updated(i, Jmp(c.instr)))
    }
    println(programs.size)
    val fixed = (programs find(m => {
      m.run()
      m.terminatedGracefully
    }) ).get
    fixed.run()
    s"The final value of the correct machine is ${fixed.accum}"
  }
}
