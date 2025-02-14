package core.csr

import chisel3._
import chisel3.util._

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
  val MSIP = Wire(Bool())
  val STIP = RegInit(false.B)
  val MTIP = Wire(Bool())
  val SEIP = RegInit(false.B)
  val MEIP = Wire(Bool())
  
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

  def sstatus = Cat(0.U(12.W), MXR, SUM,0.U(9.W),
    SPP, 0.U(2.W), SPIE, 0.U(3.W), SIE, 0.B)
  def sie = Cat(0.U(22.W), SEIE, 0.U(3.W), STIE, 0.U(3.W), SSIE, 0.B)
  def sip = Cat(0.U(22.W), SEIP, 0.U(3.W), STIP, 0.U(3.W), SSIP, 0.B)

  def mstatus = Cat(0.U(9.W), TSR, TW, TVM, MXR, SUM, MPRV, 0.U(4.W),
    MPP, 0.U(2.W), SPP, MPIE, 0.B, SPIE, 0.B, MIE, 0.B, SIE, 0.B)
  def mie = Cat(0.U(20.W), MEIE, 0.B, SEIE, 0.B, MTIE, 0.B, STIE, 0.B, MSIE, 0.B, SSIE, 0.B)
  def mip = Cat(0.U(20.W), MEIP, 0.B, SEIP, 0.B, MTIP, 0.B, STIP, 0.B, MSIP, 0.B, SSIP, 0.B)

  def intr(priv: UInt) = {
    val intrToM = !priv(1) || MIE
    val intrToS = (priv === Priv.S && SIE) || !priv(0)
    val meia = MEIP && MEIE
    val msia = MSIP && MSIE
    val mtia = MTIP && MTIE
    val seia = SEIP && SEIE
    val ssia = SSIP && SSIE
    val stia = STIP && STIE
    val meiaM = meia && ~mideleg(11)
    val msiaM = msia && ~mideleg(3)
    val mtiaM = mtia && ~mideleg(7)
    val seiaM = seia && ~mideleg(9)
    val ssiaM = ssia && ~mideleg(1)
    val stiaM = stia && ~mideleg(5)
    val seiaS = seia && mideleg(9)
    val ssiaS = ssia && mideleg(1)
    val stiaS = stia && mideleg(5)

    val intrMEn = (meiaM || msiaM || mtiaM || seiaM || ssiaM || stiaM) && intrToM
    val imtrMCause = PriorityMux(Seq(
      meiaM -> 11.U,
      msiaM -> 3.U,
      mtiaM -> 7.U,
      seiaM -> 9.U,
      ssiaM -> 1.U,
      stiaM -> 5.U
    ))
    val intrSEn = (seiaS || ssiaS || stiaS) && intrToS
    val imtrSCause = PriorityMux(Seq(
      seiaS -> 9.U,
      ssiaS -> 1.U,
      stiaS -> 5.U
    ))

    // 10：机器模式中断；01：系统模式中断；00：无中断
    val intr = Wire(UInt(2.W))
    intr := Cat(intrMEn, intrSEn && !intrMEn)
    val cause = Wire(UInt(4.W))
    cause := Mux(intrMEn, imtrMCause, imtrSCause)

    (intr, cause)
  }

  def debugOut = {
    val t = Wire(new CsrDebugBundle)
    t.stvec := stvec
    t.sscratch := sscratch
    t.sepc := sepc
    t.scause := scause
    t.stval := stval
    t.satp := satp
    t.mstatus := mstatus
    t.mtvec := mtvec
    t.mscratch := mscratch
    t.mepc := mepc
    t.mcause := mcause
    t.mtval := mtval
    t
  }
}
