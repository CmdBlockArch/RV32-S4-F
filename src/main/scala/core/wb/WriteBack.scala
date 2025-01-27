package core.wb

import chisel3._
import chisel3.util._
import core.mem.MemOut
import core.gpr.GprWriteIO
import core.csr._

class WriteBack extends Module {
  val in = IO(Flipped(Decoupled(new MemOut)))
  in.ready := true.B
  val valid = RegNext(in.valid, false.B)
  val flush = RegInit(false.B)
  val inValid = in.valid && !flush
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
}
