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
  val cur = in.bits
  val io = IO(new Bundle {
    val flush = Output(Bool())
    val dnpc = Output(UInt(32.W))
    val fenceI = Output(Bool())
    val fenceVMA = Output(Bool())

    val priv = Output(UInt(2.W))
    val satp = Output(UInt(32.W))
    val mstatusMPP = Output(UInt(2.W))
    val mstatusMPRV = Output(Bool())
    val mstatusSUM = Output(Bool())
    val mstatusMXR = Output(Bool())
    val mstatusTVM = Output(Bool())
    val mstatusTW = Output(Bool())
    val mstatusTSR = Output(Bool())
  })

  // ---------- WB流水级器件 ----------
  // CSR
  val csr = new CsrRegFile
  io.satp := csr.satp
  io.mstatusMPP := csr.MPP
  io.mstatusMPRV := csr.MPRV
  io.mstatusSUM := csr.SUM
  io.mstatusMXR := csr.MXR
  io.mstatusTVM := csr.TVM
  io.mstatusTW := csr.TW
  io.mstatusTSR := csr.TSR

  // CSR读，master位于Decode
  val csrReadIO = IO(Flipped(new CsrReadIO))
  val csrReadPort = new CsrReadPort(csr)
  val (csrReadData: UInt, csrReadErr: Bool) = csrReadPort(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr

  // 特权级
  val priv = RegInit(Priv.M)
  val privM = priv(1)
  val privS = priv === Priv.S
  io.priv := priv

  // ---------- WB动作逻辑 ----------
  // 寄存器写
  val gprWriteIO = IO(new GprWriteIO)
  gprWriteIO.en := in.valid // 流水级的valid信号
  gprWriteIO.rd := cur.rd
  gprWriteIO.data := cur.rdVal

  // CSR写
  val csrWritePort = new CsrWritePort(csr)
  when (cur.valid && cur.csrWen) {
    csrWritePort(cur.csrAddr, cur.csrData)
  }

  // flush
  io.flush := cur.valid && cur.flush
  val medeleg = csr.medeleg(15, 0)
  val trapToS = privS && medeleg(cur.cause)
  val trapAddr = Mux(trapToS, csr.stvec, csr.mtvec)
  io.dnpc := Mux1H(Seq(
    cur.mret -> csr.mepc,
    cur.sret -> csr.sepc,
    cur.trap -> trapAddr,
    (!cur.retEn && !cur.trap) -> (cur.pc + 4.U)
  ))

  // fence
  io.fenceI := cur.valid && cur.fenceI
  io.fenceVMA := cur.valid && cur.fenceVMA

  // trap
  when (cur.valid && cur.trap) {
    when (trapToS) { // trap to S-Mode
      csr.scause := cur.cause
      csr.sepc := cur.pc
      csr.stval := 0.U // TODO: stval
      csr.SPIE := csr.SIE
      csr.SIE := false.B
      csr.SPP := priv
      priv := Priv.S
    } .otherwise { // trap to M-Mode
      csr.mcause := cur.cause
      csr.mepc := cur.pc
      csr.mtval := 0.U // TODO: mtval
      csr.MPIE := csr.MIE
      csr.MIE := false.B
      csr.MPP := priv
      priv := Priv.M
    }
  }

  // ret
  when (cur.valid && cur.mret) {
    csr.MIE := csr.MPIE
    csr.MPIE := true.B
    priv := csr.MPP
    when (csr.MPP =/= Priv.M) { csr.MPRV := false.B }
    csr.MPP := Priv.U
  }
  when (cur.valid && cur.sret) {
    csr.SIE := csr.SPIE
    csr.SPIE := true.B
    priv := csr.SPP
    csr.MPRV := false.B
    csr.SPP := Priv.U
  }

  // ---------- debug ----------
  val commit = DebugRegNext(in.valid, false.B)
  val pc = DebugRegNext(cur.pc)
  val trap = DebugRegNext(cur.trap)
  val cause = DebugRegNext(cur.cause)
  val inst = DebugRegNext(cur.inst)
  val dnpc = if (debug) Some(RegNext(Mux(io.flush, io.dnpc, cur.dnpc.get), resetVec)) else None
  val skip = DebugRegNext(cur.skip)
  val debugOut = DebugIO(new WbDebugOut)
  if (debug) {
    debugOut.get.commit := commit.get
    debugOut.get.pc := pc.get
    debugOut.get.inst := inst.get
    debugOut.get.dnpc := dnpc.get
    debugOut.get.ebreak := commit.get && trap.get && cause.get === 3.U
    debugOut.get.skip := skip.get
  }
}
