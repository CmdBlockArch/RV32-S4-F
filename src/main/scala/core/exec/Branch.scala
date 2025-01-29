package core.exec

import chisel3._

class Branch extends Module {
  val io = IO(new Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val func = Input(UInt(3.W))
    val jmp = Output(Bool())
  })

  val a = io.src1
  val b = ~io.src2
  val c = (a +& b) + 1.U

  val cf = c(32)
  val sf = c(31)
  val of = (a(31) === b(31)) && (sf ^ a(31))

  val lts = sf ^ of
  val ltu = !cf
  val eq = !(io.src1 ^ io.src2).orR

  val t = Mux(io.func(2), Mux(io.func(1), ltu, lts), eq).asBool

  io.jmp := t ^ io.func(0)
}
