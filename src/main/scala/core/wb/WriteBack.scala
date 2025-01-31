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

  // valid & flush
  val flush = RegInit(false.B)
  val valid = RegNext(in.valid, false.B)
  flush := in.valid && in.bits.wbFlush

  // 特权级
  val priv = RegInit(Priv.M)
  val privM = priv(1)
  val privS = priv === Priv.S

  // ---------- GPR & CSR ----------
  // 寄存器写
  val gprWriteIO = IO(new GprWriteIO)
  gprWriteIO.en := in.valid
  gprWriteIO.rd := in.bits.rd
  gprWriteIO.data := in.bits.rdVal

  // CSR & CSR读
  val csr = new CsrRegFile
  val csrReadIO = IO(Flipped(new CsrReadIO))
  val csrReadPort = new CsrReadPort(csr)
  val (csrReadData: UInt, csrReadErr: Bool) = csrReadPort(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr

  // CSR写
  val csrWritePort = new CsrWritePort(csr)
  when (in.bits.csrWen) {
    csrWritePort(in.bits.csrAddr, in.bits.csrData)
  }

  // ---------- 流水级寄存器 ----------
  // SYS
  val ret = RegNext(in.bits.ret)
  val fenceI = RegNext(in.bits.fenceI)
  val fenceVMA = RegNext(in.bits.fenceVMA)
  // 异常
  val pc = RegNext(in.bits.pc)
  val trap = RegNext(in.bits.trap)
  val cause = RegNext(in.bits.cause)

  // ---------- 流水级逻辑 ----------
  // ret信号
  val retEn = ret.orR
  val mret = ret === "b11".U(2.W)
  val sret = ret === "b10".U(2.W)
  // trap信号
  val medeleg = Cat(csr.medelegh, csr.medeleg)
  val trapToS = privS && medeleg(cause)
  val trapAddr = Mux(trapToS, csr.stvec, csr.mtvec)

  // fence & flush
  val io = IO(new Bundle {
    val flush = Output(Bool())
    val dnpc = Output(UInt(32.W))
    val fenceI = Output(Bool())
    val fenceVMA = Output(Bool())

    val priv = Output(UInt(2.W))
    val mstatusTSR = Output(Bool())
    val mstatusTVM = Output(Bool())
    val mstatusTW = Output(Bool())
  })
  io.flush := flush
  io.dnpc := Mux1H(Seq(
    mret -> csr.mepc,
    sret -> csr.sepc,
    trap -> trapAddr,
    (!retEn && !trap) -> (pc + 4.U)
  ))
  io.fenceI := valid && fenceI
  io.fenceVMA := valid && fenceVMA
  io.priv := priv
  io.mstatusTSR := csr.TSR
  io.mstatusTVM := csr.TVM
  io.mstatusTW := csr.TW

  // trap
  when (valid && trap) {
    when (trapToS) { // trap to S-Mode
      csr.scause := cause
      csr.sepc := pc
      csr.stval := 0.U // TODO: stval
      csr.SPIE := csr.SIE
      csr.SIE := false.B
      csr.SPP := priv
      priv := Priv.S
    } .otherwise { // trap to M-Mode
      csr.mcause := cause
      csr.mepc := pc
      csr.mtval := 0.U // TODO: mtval
      csr.MPIE := csr.MIE
      csr.MIE := false.B
      csr.MPP := priv
      priv := Priv.M
    }
  }

  // ret
  when (valid && mret) {
    csr.MIE := csr.MPIE
    csr.MPIE := true.B
    priv := csr.MPP
    when (csr.MPP =/= Priv.M) { csr.MPRV := false.B }
    csr.MPP := Priv.U
  }
  when (valid && sret) {
    csr.SIE := csr.SPIE
    csr.SPIE := true.B
    priv := csr.SPP
    csr.MPRV := false.B
    csr.SPP := Priv.U
  }

  // ---------- debug ----------
  val inst = if (debug) Some(RegNext(in.bits.inst.get)) else None
  val dnpc = if (debug) Some(RegNext(in.bits.dnpc.get, 0x80000000L.U(32.W))) else None
  val skip = if (debug) Some(RegNext(in.bits.skip.get)) else None
  val debugOut = if (debug) Some(IO(new WbDebugOut)) else None
  if (debug) {
    debugOut.get.commit := valid
    debugOut.get.pc := pc
    debugOut.get.inst := inst.get
    debugOut.get.dnpc := Mux(io.flush, io.dnpc, dnpc.get)
    debugOut.get.ebreak := valid && trap && cause === 3.U
    debugOut.get.skip := skip.get
  }
}
