package core.exec

import chisel3._
import chisel3.util._
import core.decode.DecodeOut
import utils.PiplineModule

class ExecOut extends Bundle {

}

class Exec extends PiplineModule(new DecodeOut, new ExecOut) {
  override def outCond = true.B

  val io = IO(new Bundle {
    val rd = Output(UInt(5.W))
    val fwReady = Output(Bool())

    val jmp = Output(Bool())
    val dnpc = Output(UInt(32.W))
  })

  // ALU
  val alu = Module(new Alu)
  alu.io.a := Mux1H(cur.valASel, Seq(0.U(32.W), cur.pc, cur.src1))
  alu.io.b := Mux1H(cur.valBSel, Seq(4.U(32.W), cur.imm, cur.src2))
  alu.func := cur.aluFunc
  val aluRes = alu.io.res

  // 前递
  io.rd := cur.rd
  io.fwReady := valid && cur.fwReady

  // 分支和跳转
  val brJmp = alu.io.jmp
  io.jmp := valid && (cur.jal || cur.jalr || (cur.branch && brJmp))
  io.dnpc := Mux(cur.jalr, cur.src1, cur.pc) + Mux(cur.branch && !brJmp, 4.U(32.W), cur.imm)


}
