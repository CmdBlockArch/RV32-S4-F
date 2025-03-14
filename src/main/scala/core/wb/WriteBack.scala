package core.wb

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}
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
  val csr = Output(new CsrDebugBundle)
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

    val msip = Input(Bool())
    val mtip = Input(Bool())
    val meip = Input(Bool())
    val intr = Output(UInt(2.W))
    val intrCause = Output(UInt(32.W))
  })

  // ---------- WB流水级器件 ----------
  // 特权级
  val priv = RegInit(Priv.M)
  val privM = priv(1)
  io.priv := priv

  // CSR
  val csr = new CsrRegFile
  csr.counter := csr.counter + 1.U
  io.satp := csr.satp
  io.mstatusMPP := csr.MPP
  io.mstatusMPRV := csr.MPRV
  io.mstatusSUM := csr.SUM
  io.mstatusMXR := csr.MXR
  io.mstatusTVM := csr.TVM
  io.mstatusTW := csr.TW
  io.mstatusTSR := csr.TSR

  // 中断
  csr.MSIP := io.msip
  csr.MTIP := io.mtip
  csr.MEIP := io.meip
  val (outIntr, outIntrCause) = csr.intr(priv)
  io.intr := outIntr
  io.intrCause := outIntrCause

  // CSR读，master位于Decode
  val csrReadIO = IO(Flipped(new CsrReadIO))
  val csrReadPort = new CsrReadPort(csr)
  val (csrReadData: UInt, csrReadErr: Bool) = csrReadPort(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr

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

  // 中断
  val intr = cur.intr.orR
  val intrTvec = Mux1H(cur.intr, Seq(csr.stvec, csr.mtvec))
  val intrAddr = Cat(intrTvec(31, 2), 0.U(2.W)) + Mux(intrTvec(0), Cat(cur.cause, 0.U(2.W)), 0.U)

  // 异常
  val medeleg = csr.medeleg(15, 0)
  val trapToS = !privM && medeleg(cur.cause)
  val trapTvec = Mux(trapToS, csr.stvec, csr.mtvec)
  val trapAddr = Cat(trapTvec(31, 2), 0.U(2.W))

  // flush
  io.flush := cur.valid && cur.flush
  io.dnpc := Mux1H(Seq(
    cur.mret -> csr.mepc,
    cur.sret -> csr.sepc,
    cur.trap -> Mux(intr, intrAddr, trapAddr),
    (!cur.retEn && !cur.trap) -> (cur.pc + 4.U)
  ))

  // fence
  io.fenceI := cur.valid && cur.fenceI
  io.fenceVMA := cur.valid && cur.fenceVMA

  val tval = Mux(intr, 0.U, Mux1H(decoder(cur.cause, WriteBack.tvalTruthTable), Seq(0.U(32.W), cur.pc, cur.vaddr)))

  // trap
  when (cur.valid && cur.trap) {
    when (Mux(intr, cur.intr(0), trapToS)) { // trap to S-Mode
      csr.scause := Cat(intr.asBool, 0.U(27.W), cur.cause)
      csr.sepc := cur.pc
      csr.stval := tval
      csr.SPIE := csr.SIE
      csr.SIE := false.B
      csr.SPP := priv
      priv := Priv.S
    } .otherwise { // trap to M-Mode
      csr.mcause := Cat(intr.asBool, 0.U(27.W), cur.cause)
      csr.mepc := cur.pc
      csr.mtval := tval
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
  val dnpc = if (debug) Some(RegNext(Mux(io.flush, io.dnpc, cur.dnpc), resetVec)) else None
  val skip = DebugRegNext(cur.skip)
  val debugOut = DebugIO(new WbDebugOut)
  if (debug) {
    debugOut.get.commit := commit.get
    debugOut.get.pc := pc.get
    debugOut.get.inst := inst.get
    debugOut.get.dnpc := dnpc.get
    debugOut.get.ebreak := commit.get && trap.get && cause.get === 3.U
    debugOut.get.skip := skip.get
    debugOut.get.csr := csr.debugOut
  }
}

object WriteBack {
  val tvalTruthTable = TruthTable(
    Map(
      BitPat("b0000") -> BitPat("b010"),
      BitPat("b0001") -> BitPat("b010"),
      BitPat("b0010") -> BitPat("b001"),
      BitPat("b0011") -> BitPat("b010"),
      BitPat("b0100") -> BitPat("b100"),
      BitPat("b0101") -> BitPat("b100"),
      BitPat("b0110") -> BitPat("b100"),
      BitPat("b0111") -> BitPat("b100"),
      BitPat("b1000") -> BitPat("b001"),
      BitPat("b1001") -> BitPat("b001"),
      BitPat("b1011") -> BitPat("b001"),
      BitPat("b1100") -> BitPat("b010"),
      BitPat("b1101") -> BitPat("b100"),
      BitPat("b1111") -> BitPat("b100"),
    ), BitPat.dontCare(3)
  )
}
