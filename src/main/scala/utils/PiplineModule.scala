package utils

import chisel3._
import chisel3.util._

abstract class PiplineModule[TI <: Data, TO <: Data]
(val inType: TI, val outType: TO) extends Module {
  val in = IO(Flipped(Decoupled(inType)))
  val out = IO(Decoupled(outType))
  val flush = IO(Input(Bool()))

  val valid = RegInit(false.B)
  val cur = Reg(inType)

  in.ready := !valid || out.fire
  out.valid := valid && !flush
  when (in.fire) {
    valid := true.B
    cur := in.bits
  } .elsewhen (out.fire || flush) {
    valid := false.B
  }

  def setOutCond(cond: Bool) = {
    out.valid := valid && !flush && cond
  }
}
