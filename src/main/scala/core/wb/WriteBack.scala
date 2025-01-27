package core.wb

import chisel3._
import chisel3.util._
import core.mem.MemOut
import core.gpr.GprWriteIO
import core.csr._

class WriteBack extends Module {
  val in = IO(Flipped(Decoupled(new MemOut)))
  in.ready := true.B

  // 寄存器写
  val gprWriteIO = IO(new GprWriteIO)
  gprWriteIO.en := in.valid
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
  when (in.valid && in.bits.csrWen) {
    csrWritePort(in.bits.csrAddr, in.bits.csrData)
  }

  // ret
  val ret = in.bits.ret.orR
  val mret = in.bits.ret === "b11".U(2.W)
  val sret = in.bits.ret === "b10".U(2.W)

  // trap
  val trap = in.bits.trap
  val trapAddr = csrRegFile.mtval // TODO: mtval or stval

  // fence & flush
  val flush = in.bits.csrWen || ret || trap || in.bits.fenceI || in.bits.fenceVMA
  val io = IO(new Bundle {
    val flush = Output(Bool())
    val dnpc = Output(UInt(32.W))
    val fenceI = Output(Bool())
    val fenceVMA = Output(Bool())
  })
  io.flush := in.valid && flush
  io.dnpc := Mux1H(Seq(
    mret -> csrRegFile.mepc,
    sret -> csrRegFile.sepc,
    trap -> trapAddr,
    (!ret && !trap) -> (in.bits.pc + 4.U)
  ))
  io.fenceI := in.valid && in.bits.fenceI
  io.fenceVMA := in.valid && in.bits.fenceVMA
}
