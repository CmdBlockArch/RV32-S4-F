package core.csr

import chisel3._

class CsrReadIO extends Bundle {
  val addr = Output(UInt(12.W))
  val data = Input(UInt(32.W))
  val err = Input(Bool())
}

class CsrDebugBundle extends Bundle {
  val stvec = UInt(32.W)
  val sscratch = UInt(32.W)
  val sepc = UInt(32.W)
  val scause = UInt(32.W)
  val stval = UInt(32.W)
  val satp = UInt(32.W)
  val mstatus = UInt(32.W)
  val mtvec = UInt(32.W)
  val mscratch = UInt(32.W)
  val mepc = UInt(32.W)
  val mcause = UInt(32.W)
  val mtval = UInt(32.W)
}
