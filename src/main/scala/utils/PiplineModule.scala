package utils

import chisel3._
import chisel3.util._

abstract class PiplineModule[TI <: Data, TO <: Data](inType: TI, outType: TO) extends Module {
  val in = IO(Flipped(Decoupled(inType)))
  val out = IO(Decoupled(outType))
  val flush = IO(Input(Bool()))

  val valid = RegInit(false.B)
  val cur = Reg(inType)
  def outCond: Bool

  in.ready := !valid || out.fire
  out.valid := valid && outCond
  when (in.fire) {
    valid := !flush
    cur := in.bits
  } .elsewhen (out.fire) {
    valid := false.B
  }
  when (flush) {
    valid := false.B
  }
}
