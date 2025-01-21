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

class GprFwIO extends Bundle {
  val valid = Output(Bool())
  val rd = Output(UInt(5.W))
  val fwVal = Output(UInt(32.W))
}

class RegFile extends Module {
  val readIO = IO(Flipped(new GprReadIO))
  val writeIO = IO(Flipped(new GprWriteIO))

  val execFwIO = IO(new GprFwIO)
  val memFwIO = IO(new GprFwIO)

  val regs = Reg(Vec(32, UInt(32.W))) // 0号寄存器会被自动优化

  def readGpr(rs: UInt): UInt = {
    val src = Wire(UInt(32.W))
    when (rs === 0.U(5.W)) {
      src := 0.U(32.W)
    } .elsewhen (execFwIO.valid && execFwIO.rd === rs) {
      src := execFwIO.fwVal
    } .elsewhen (memFwIO.valid && memFwIO.rd === rs) {
      src := memFwIO.fwVal
    } .otherwise {
      src := regs(rs)
    }
    src
  }

  readIO.src1 := readGpr(readIO.src1)
  readIO.src2 := readGpr(readIO.src2)

  when(writeIO.rd =/= 0.U) {
    regs(writeIO.rd) := writeIO.data
  }
}
