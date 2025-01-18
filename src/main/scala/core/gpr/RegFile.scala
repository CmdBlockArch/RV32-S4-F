package core.gpr

import chisel3._

class GprReadIO extends Bundle {
  val rs1 = Output(UInt(5.W))
  val rs2 = Output(UInt(5.W))
  val src1 = Input(UInt(32.W))
  val src2 = Input(UInt(32.W))
}

class GprWriteIO extends Bundle {
  val rd = Output(UInt(5.W))
  val data = Output(UInt(32.W))
}

class RegFile extends Module {
  val readIO = IO(Flipped(new GprReadIO))
  val writeIO = IO(Flipped(new GprWriteIO))

  val regs = Reg(Vec(32, UInt(32.W))) // 0号寄存器会被自动优化

  readIO.src1 := Mux(readIO.rs1 === 0.U, 0.U, regs(readIO.rs1))
  readIO.src2 := Mux(readIO.rs2 === 0.U, 0.U, regs(readIO.rs2))

  when(writeIO.rd =/= 0.U) {
    regs(writeIO.rd) := writeIO.data
  }
}
