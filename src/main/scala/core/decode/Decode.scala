package core.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodeTable
import core.csr.CsrReadIO
import core.fetch.FetchOut
import core.fetch.Bpu.{factory => bpu}
import core.gpr.GprReadIO
import core.csr.Priv
import utils.PiplineModule
import utils.Config._

class DecodeOut extends Bundle {
  // 运算数
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
  val imm = Output(UInt(32.W))
  val func = Output(UInt(3.W))
  // ALU选数/功能
  val valASel = Output(UInt(3.W))
  val valBSel = Output(UInt(3.W))
  val aluFunc = Output(new FuncBundle)
  // ALU前递路径
  val rd = Output(UInt(5.W))
  val fwReady = Output(Bool()) // rd是否可以直接前递
  // 分支和跳转
  val branch = Output(Bool())
  val jmp = Output(Bool())
  val jalr = Output(Bool())
  val rasPush = Output(Bool())
  val rasPop = Output(Bool())
  // 功能单元控制
  val mul = Output(Bool())
  val mem = Output(UInt(4.W))
  val amoFunc = Output(UInt(4.W))
  // CSR
  val zicsr = Output(Bool())
  val csrWen = Output(Bool())
  val csrAddr = Output(UInt(12.W))
  val csrSrc = Output(UInt(32.W))
  // SYS
  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())
  // 异常处理
  val pc = Output(UInt(32.W))
  val trap = Output(Bool())
  val intr = Output(UInt(2.W))
  val cause = Output(UInt(4.W))
  // 分支预测
  val bpuHit = Output(Bool())
  val bpuIdx = Output(UInt(bpu.btbW.W))
  val dnpc = Output(UInt(32.W))
  // 调试
  val inst = DebugOutput(UInt(32.W))
  val skip = DebugOutput(Bool())
}

class Decode extends PiplineModule(new FetchOut, new DecodeOut) {
  // 译码指令位域
  val inst = cur.inst
  val func3 = cur.inst(14, 12)
  val calSign = cur.inst(30)
  val rs1 = cur.inst(19, 15)
  val rs2 = cur.inst(24, 20)
  val rd = cur.inst(11, 7)
  out.bits.func := func3

  // 从指令译码控制信号
  val decodeTable = new DecodeTable(InstPattern.patterns, InstField.fields)
  val table = decodeTable.table
  val cs = decodeTable.decode(inst) // 控制信号

  // 译码GPR操作数
  val gprReadIO = IO(new GprReadIO)
  gprReadIO.rs1 := rs1
  gprReadIO.rs2 := rs2
  out.bits.src1 := gprReadIO.src1
  out.bits.src2 := gprReadIO.src2
  setOutCond(gprReadIO.src1Ready && gprReadIO.src2Ready)

  // 译码立即数
  val immI = Cat(Fill(20, inst(31)), inst(31, 20))
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7))
  val immB = Cat(Fill(20, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
  val immU = Cat(inst(31, 12), 0.U(12.W))
  val immJ = Cat(Fill(12, inst(31)), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
  val immZ = Cat(0.U(27.W), inst(19, 15))
  out.bits.imm := Mux1H(cs(ImmSelField), Seq(immI, immS, immB, immU, immJ, immZ))

  // ALU
  out.bits.valASel := cs(ValASelField)
  out.bits.valBSel := cs(ValBSelField)
  val aluFuncEn = cs(AluFuncEnField)
  val aluSignEn = cs(AluSignEnField)
  val branch = out.bits.branch
  out.bits.aluFunc := FuncBundle.decodeFunc(
    Mux(aluFuncEn, func3, 0.U(3.W)), (aluSignEn && calSign) || branch)

  // rd和前递
  out.bits.rd := Mux(cs(RdEnField), rd, 0.U(5.W))
  out.bits.fwReady := cs(FwReadyField)

  // 分支和跳转
  val jmpFlag = Mux(cur.trap, 0.U(3.W), cs(JmpField))
  val jal = jmpFlag(0)
  val jalr = jmpFlag(1)
  out.bits.jmp := jal || jalr
  out.bits.jalr := jalr
  out.bits.branch := jmpFlag(2)
  val rdLink = rd === 1.U || rd === 5.U
  val rs1Link = rs1 === 1.U || rs1 === 5.U
  val rders1 = rd === rs1
  out.bits.rasPush := rdLink && (jal || jalr)
  out.bits.rasPop := jalr && rs1Link && !rders1

  // 功能单元控制
  out.bits.mul := cs(MulField) && !cur.trap
  out.bits.mem := Mux(cur.trap, 0.U, cs(MemField))
  out.bits.amoFunc := Cat(inst(31, 29), inst(27))

  // io
  val io = IO(new Bundle {
    val priv = Input(UInt(2.W))
    val mstatusTVM = Input(Bool())
    val mstatusTW = Input(Bool())
    val mstatusTSR = Input(Bool())
  })

  // CSR
  val zicsr = cs(ZicsrEnField) && !cur.trap
  out.bits.zicsr := zicsr
  val csrWen = (!func3(1) || Mux(func3(2), immZ.orR, rs1.orR)) && !cur.trap
  out.bits.csrWen := csrWen
  val csrAddr = inst(31, 20)
  out.bits.csrAddr := csrAddr
  val csrReadIO = IO(new CsrReadIO)
  csrReadIO.addr := csrAddr
  out.bits.csrSrc := csrReadIO.data
  val csrWriteRO = csrAddr(11, 10).andR && csrWen
  val csrPriv = csrAddr(9, 8)
  val csrAddrErr = csrWriteRO || csrReadIO.err
  val csrPrivErr = (io.priv === Priv.U && csrPriv.orR) || (io.priv === Priv.S && csrPriv(1))
  val csrErr = zicsr && (csrAddrErr || csrPrivErr)

  // ret
  val ret = Mux(cur.trap, 0.U, cs(RetField))
  out.bits.ret := ret
  val retErr = MuxLookup(ret, false.B)(Seq(
    Priv.M -> (io.priv =/= Priv.M),
    Priv.S -> (io.priv === Priv.U || (io.priv === Priv.S && io.mstatusTSR))
  ))
  // fence
  val fenceVMA = cs(FenceVMAField) && !cur.trap
  out.bits.fenceVMA := fenceVMA
  val fenceI = cs(FenceIField) && !cur.trap
  out.bits.fenceI := fenceI || fenceVMA
  val fenceVMAErr = fenceVMA && (io.priv === Priv.U || (io.priv === Priv.S && io.mstatusTVM))
  // ecall & ebreak
  val ecall = cs(EcallField) && !cur.trap
  val ebreak = cs(EbreakField) && !cur.trap
  // wfi
  val wfi = cs(WfiField) && !cur.trap
  val wfiErr = wfi && io.priv === Priv.U && io.mstatusTW

  // trap
  val invInst = cs(InvInstField) || csrErr || retErr || fenceVMAErr || wfiErr
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap || invInst || ecall || ebreak
  out.bits.intr := cur.intr
  out.bits.cause := Mux(cur.trap, cur.cause, Mux1H(Seq(
    invInst -> 2.U,
    ecall -> (8.U(4.W) | io.priv),
    ebreak -> 3.U,
  )))

  // 分支预测
  out.bits.bpuHit := cur.bpuHit
  out.bits.bpuIdx := cur.bpuIdx
  out.bits.dnpc := cur.dnpc

  if (debug) {
    val skipCSR = VecInit(Seq("h301".U, "hc00".U, "hc01".U, "hc02".U, "hc80".U, "hc81".U, "hc82".U))
    out.bits.inst.get := inst
    out.bits.skip.get := cur.skip.get || (zicsr && skipCSR.contains(csrAddr))
  }
}
