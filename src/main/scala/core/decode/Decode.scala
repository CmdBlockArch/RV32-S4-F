package core.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodeTable
import utils.PiplineModule
import core.fetch.FetchOut
import core.gpr.GprReadIO

class DecodeOut extends Bundle {
  val valA = Output(UInt(32.W))
  val valB = Output(UInt(32.W))
  val valC = Output(UInt(32.W))

  val func = Output(UInt(4.W))
  val mul = Output(Bool())
  val rd = Output(UInt(5.W))

  val mem = Output(UInt(4.W))

  val zicsr = Output(UInt(2.W))
  val csrAddr = Output(UInt(12.W))

  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())

  val pc = Output(UInt(32.W))
  val trap = Output(Bool())
  val cause = Output(UInt(4.W))
}

class Decode extends PiplineModule(new FetchOut, new DecodeOut) {
  val io = IO(new Bundle {
    val jmp = Output(Bool())
    val dnpc = Output(UInt(32.W))
  })

  // 译码指令
  val func3 = cur.inst(14, 12)
  val funcs = cur.inst(30)
  val rs1 = cur.inst(19, 15)
  val rs2 = cur.inst(24, 20)
  val rd = cur.inst(11, 7)

  // 译码寄存器
  val gprReadIO = IO(new GprReadIO)
  gprReadIO.rs1 := rs1
  gprReadIO.rs2 := rs2
  val src1 = gprReadIO.src1
  val src2 = gprReadIO.src2

  // 译码立即数
  val immI = Cat(Fill(20, cur.inst(31)), cur.inst(31, 20))
  val immS = Cat(Fill(20, cur.inst(31)), cur.inst(31, 25), cur.inst(11, 7))
  val immB = Cat(Fill(20, cur.inst(31)), cur.inst(7), cur.inst(30, 25), cur.inst(11, 8), 0.U(1.W))
  val immU = Cat(cur.inst(31, 12), 0.U(12.W))
  val immJ = Cat(Fill(12, cur.inst(31)), cur.inst(19, 12), cur.inst(20), cur.inst(30, 21), 0.U(1.W))
  val immZ = Cat(0.U(27.W), cur.inst(19, 15))

  // 译码控制信号
  val decodeTable = new DecodeTable(InstPattern.patterns, InstField.fields)
  val table = decodeTable.table
  val decodeRes = decodeTable.decode(cur.inst)
  val invInst = decodeRes(InvInstField)
  out.bits.func := Mux(decodeRes(FuncEnField), Cat(funcs && decodeRes(SignEnField), func3), 0.U(4.W))
  out.bits.mul := decodeRes(MulField)
  out.bits.rd := Mux(decodeRes(RdEnField), rd, 0.U(5.W))
  out.bits.mem := decodeRes(MemField)
  out.bits.zicsr := Mux(decodeRes(ZicsrEnField), func3(1, 0), 0.U(2.W))
  out.bits.csrAddr := cur.inst(31, 20)
  out.bits.ret := decodeRes(RetField)
  out.bits.fenceI := decodeRes(fenceIField)
  out.bits.fenceVMA := decodeRes(fenceVMAField)

  // 选数
  out.bits.valA := Mux1H(decodeRes(valASelField),
    Seq(0.U(32.W), cur.pc, src1))
  out.bits.valB := Mux1H(decodeRes(ValBSelField),
    Seq(4.U(32.W), immU, immI, src2))
  out.bits.valC := Mux1H(decodeRes(ValCSelField), Seq(
    src1 + Mux(decodeRes(MemField)(3), immI, immS),
    src1, immZ))

  // trap
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap || invInst
  out.bits.cause := Mux(cur.trap, cur.cause, 2.U) // 2: illegal instruction

  // 跳转
  val jmpFlag = decodeRes(JmpField)
  val jal = jmpFlag(0)
  val jalr = jmpFlag(1)
  val branch = jmpFlag(2)

  val bru = Module(new BranchUnit)
  bru.io.src1 := src1
  bru.io.src2 := src2
  bru.io.func := func3
  val branchJmpEn = bru.io.jmp

  io.jmp := valid && ((branch && branchJmpEn) || jal || jalr)
  io.dnpc := Mux(jalr, src1, cur.pc) + Mux1H(jmpFlag, Seq(immJ, immI, immB))
}
