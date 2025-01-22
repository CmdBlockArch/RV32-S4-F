package core.csr

import chisel3._

class CsrRegFile {
  // mstatus(sstatus)
  val SIE = RegInit(false.B)
  val MIE = RegInit(false.B)
  val SPIE = RegInit(false.B)
  val MPIE = RegInit(false.B)
  val SPP = RegInit(false.B)
  val MPP = RegInit(0.U(2.W))
  val MPRV = RegInit(false.B)
  val SUM = RegInit(false.B)
  val MXR = RegInit(false.B)
  val TVM = RegInit(false.B)
  val TW = RegInit(false.B)
  val TSR = RegInit(false.B)

  // mie
  val SSIE = RegInit(false.B)
  val MSIE = RegInit(false.B)
  val STIE = RegInit(false.B)
  val MTIE = RegInit(false.B)
  val SEIE = RegInit(false.B)
  val MEIE = RegInit(false.B)
  
  // mip
  val SSIP = RegInit(false.B)
  val MSIP = RegInit(false.B)
  val STIP = RegInit(false.B)
  val MTIP = RegInit(false.B)
  val SEIP = RegInit(false.B)
  val MEIP = RegInit(false.B)
  
  val mtvec = Reg(UInt(32.W))
  val mepc = Reg(UInt(32.W))
  val mcause = RegInit(0.U(32.W))
  val mtval = Reg(UInt(32.W))
  val medeleg = RegInit(0.U(32.W))
  val medelegh = RegInit(0.U(32.W))
  val mideleg = RegInit(0.U(32.W))

  val satp = RegInit(0.U(32.W))
  val mscratch = Reg(UInt(32.W))
  val sscratch = Reg(UInt(32.W))
  val stvec = Reg(UInt(32.W))
  val sepc = Reg(UInt(32.W))
  val scause = Reg(UInt(32.W))
  val stval = Reg(UInt(32.W))

  val counter = RegInit(0.U(64.W))
}
