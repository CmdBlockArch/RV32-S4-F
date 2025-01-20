package core.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodeTable
import core.csr.CsrReadIO
import utils.PiplineModule
import core.fetch.FetchOut
import core.gpr.GprReadIO
import core.exec.Alu

class DecodeOut extends Bundle {
  // 运算数
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
  val imm = Output(UInt(32.W))
  // ALU选数/功能
  val valASel = Output(UInt(3.W))
  val valBSel = Output(UInt(3.W))
  val aluFunc = Output(new Alu.FuncBundle) // 分支类型复用
  // ALU前递路径
  val rd = Output(UInt(5.W))
  val fwReady = Output(Bool()) // rd是否可以直接前递
  // 分支和跳转
  val branch = Output(Bool())
  val jal = Output(Bool())
  val jalr = Output(Bool())
  // 功能单元控制
  val mul = Output(Bool())
  val mem = Output(UInt(4.W))
  val amoFunc = Output(UInt(4.W))
  // CSR
  val zicsr = Output(UInt(3.W))
  val csrAddr = Output(UInt(12.W))
  val csrSrc = Output(UInt(32.W))
  // SYS
  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())
  // 异常处理
  val pc = Output(UInt(32.W))
  val trap = Output(Bool())
  val cause = Output(UInt(4.W))
}

class Decode extends PiplineModule(new FetchOut, new DecodeOut) {
  // 译码指令位域
  val inst = cur.inst
  val func3 = cur.inst(14, 12)
  val calSign = cur.inst(30)
  val rs1 = cur.inst(19, 15)
  val rs2 = cur.inst(24, 20)
  val rd = cur.inst(11, 7)

  // 从指令译码控制信号
  val decodeTable = new DecodeTable(InstPattern.patterns, InstField.fields)
  val table = decodeTable.table
  val cs = decodeTable.decode(inst) // 控制信号

  // 译码GPR操作数
  // TODO: 前递
  val gprReadIO = IO(new GprReadIO)
  gprReadIO.rs1 := rs1
  gprReadIO.rs2 := rs2
  out.bits.src1 := gprReadIO.src1
  out.bits.src2 := gprReadIO.src2

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
  out.bits.aluFunc := Alu.decodeFunc(
    Mux(aluFuncEn, func3, 0.U(3.W)), Mux(aluSignEn, calSign, false.B))

  // rd和前递
  out.bits.rd := Mux(cs(RdEnField), rd, 0.U(5.W))
  out.bits.fwReady := cs(FwReadyField)

  // 分支和跳转
  val jmpFlag = cs(JmpField)
  out.bits.jal := jmpFlag(0)
  out.bits.jalr := jmpFlag(1)
  out.bits.branch := jmpFlag(2)

  // 功能单元控制
  out.bits.mul := cs(MulField)
  out.bits.mem := cs(MemField)
  out.bits.amoFunc := Cat(inst(31, 29), inst(27))

  // CSR
  out.bits.zicsr := Mux(cs(ZicsrEnField), func3(1, 0), 0.U(2.W))
  val csrAddr = inst(31, 20)
  out.bits.csrAddr := csrAddr
  val csrReadIO = IO(new CsrReadIO)
  csrReadIO.data := csrAddr
  out.bits.csrSrc := csrReadIO.data

  // SYS
  out.bits.ret := cs(RetField)
  out.bits.fenceI := cs(FenceIField)
  out.bits.fenceVMA := cs(FenceVMAField)

  // trap
  val invInst = cs(InvInstField) || csrReadIO.err
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap || invInst
  out.bits.cause := Mux(cur.trap, cur.cause, 2.U) // 2: illegal instruction
}
