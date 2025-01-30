package core.wb

import chisel3._
import chisel3.util._
import core.mem.MemOut
import core.gpr.GprWriteIO
import core.csr._
import utils.Config._

class WbDebugOut extends Bundle {
  val commit = Output(Bool())
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
  val dnpc = Output(UInt(32.W))
  val ebreak = Output(Bool())
  val skip = Output(Bool())
}

class WriteBack extends Module {
  val in = IO(Flipped(Decoupled(new MemOut)))
  in.ready := true.B

  val flush = RegInit(false.B)
  val inValid = in.valid && !flush
  val valid = RegNext(inValid, false.B)
  flush := inValid && in.bits.wbFlush

  // 寄存器写
  val gprWriteIO = IO(new GprWriteIO)
  gprWriteIO.en := inValid
  gprWriteIO.rd := in.bits.rd
  gprWriteIO.data := in.bits.rdVal

  // 特权级状态
  val priv = RegInit(Priv.M)
  val privM = priv(1)
  val privS = priv === Priv.S

  // CSR & CSR读
  val csrRegFile = new CsrRegFile
  val csrReadIO = IO(Flipped(new CsrReadIO))
  val csrReadPort = new CsrReadPort(csrRegFile)
  val (csrReadData: UInt, csrReadErr: Bool) = csrReadPort(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr

  // CSR写
  val csrWritePort = new CsrWritePort(csrRegFile)
  when (inValid && in.bits.csrWen) {
    csrWritePort(in.bits.csrAddr, in.bits.csrData)
  }

  // ------------------------------
  // SYS
  val ret = RegNext(in.bits.ret)
  val fenceI = RegNext(in.bits.fenceI)
  val fenceVMA = RegNext(in.bits.fenceVMA)
  // 异常
  val pc = RegNext(in.bits.pc)
  val trap = RegNext(in.bits.trap)
  val cause = RegNext(in.bits.cause)

  val retEn = ret.orR
  val mret = ret === "b11".U(2.W)
  val sret = ret === "b10".U(2.W)
  val trapAddr = csrRegFile.mtval // TODO: mtval or stval

  // fence & flush
  val io = IO(new Bundle {
    val flush = Output(Bool())
    val dnpc = Output(UInt(32.W))
    val fenceI = Output(Bool())
    val fenceVMA = Output(Bool())
  })
  io.flush := flush
  io.dnpc := Mux1H(Seq(
    mret -> csrRegFile.mepc,
    sret -> csrRegFile.sepc,
    trap -> trapAddr,
    (!retEn && !trap) -> (pc + 4.U)
  ))
  io.fenceI := valid && fenceI
  io.fenceVMA := valid && fenceVMA

  // debug
  val inst = if (debug) Some(RegNext(in.bits.inst.get)) else None
  val dnpc = if (debug) Some(RegNext(in.bits.dnpc.get, 0x80000000L.U(32.W))) else None
  val skip = if (debug) Some(RegNext(in.bits.skip.get)) else None
  val debugOut = if (debug) Some(IO(new WbDebugOut)) else None
  if (debug) {
    debugOut.get.commit := valid
    debugOut.get.pc := pc
    debugOut.get.inst := inst.get
    debugOut.get.dnpc := dnpc.get
    debugOut.get.ebreak := valid && trap && cause === 3.U
    debugOut.get.skip := skip.get
  }
}
