package core.exec

import chisel3._
import chisel3.util._
import core.decode.DecodeOut
import utils.{PiplineModule, MuxLookup1H}

class ExecOut extends Bundle {
  val rd = Output(UInt(5.W))
  val rdVal = Output(UInt(32.W))
  val fwReady = Output(Bool())
  // MEM
  val mem = Output(UInt(4.W))
  val amoFunc = Output(UInt(4.W))
  // CSR
  val csrWen = Output(Bool())
  val csrAddr = Output(UInt(12.W))
  val csrData = Output(UInt(32.W))
  // SYS
  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())
  // 异常处理
  val pc = Output(UInt(32.W))
  val trap = Output(Bool())
  val cause = Output(UInt(4.W))
}

class Exec extends PiplineModule(new DecodeOut, new ExecOut) {
  val io = IO(new Bundle {
    val rd = Output(UInt(5.W))
    val fwReady = Output(Bool())
    val fwVal = Input(UInt(32.W))

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
  io.fwVal := aluRes

  // 分支和跳转
  val brJmp = alu.io.jmp
  io.jmp := valid && (cur.jal || cur.jalr || (cur.branch && brJmp))
  io.dnpc := Mux(cur.jalr, cur.src1, cur.pc) + Mux(cur.branch && !brJmp, 4.U(32.W), cur.imm)

  // 乘除法
  val mulFunc = cur.aluFunc.mulFunc
  val mulFuncMul = mulFunc.mul || mulFunc.mulh || mulFunc.mulhsu || mulFunc.mulhu

  val mul = Module(new Mul)
  mul.in.valid := valid && !cur.trap && cur.mul && mulFuncMul
  mul.in.sign := Cat(mulFunc.mulh || mulFunc.mulhsu, mulFunc.mulh)
  mul.in.a := cur.src1
  mul.in.b := cur.src2
  mul.out.ready := out.ready
  mul.flush := flush
  val mulVal = Mux(mulFunc.mul, mul.out.prod(31, 0), mul.out.prod(63, 32))
  val mulValValid = mul.out.valid

  val div = Module(new Div)
  val divOf = cur.src1 === Cat(1.U(1.W), 0.U(31.W)) && cur.src2.andR
  val divZero = cur.src2 === 0.U(32.W)
  val divSign = mulFunc.div || mulFunc.rem
  div.in.valid := valid && !cur.trap && cur.mul && !divZero &&
    ((divSign && !divOf) || mulFunc.divu || mulFunc.remu)
  div.in.sign := divSign
  div.in.a := cur.src1
  div.in.b := cur.src2
  div.out.ready := out.ready
  div.flush := flush
  val divVal = Mux1H(Seq(
    mulFunc.div -> Mux(divOf || divZero, Cat(1.U(1.W), Fill(31, divZero)), div.out.quot),
    mulFunc.divu -> Mux(divZero, Fill(32, 1.U(1.W)), div.out.quot),
    mulFunc.rem -> Mux(divOf || divZero, cur.src1 & Fill(32, divZero), div.out.rem),
    mulFunc.remu -> Mux(divZero, cur.src1, div.out.rem),
  ))
  val divValValid = div.out.valid || divZero || (divSign && divOf)

  // CSR
  val zicsr = cur.zicsr(1, 0).orR
  val csrOpnd = Mux(cur.zicsr(2), cur.imm, cur.src1)
  out.bits.csrAddr := cur.csrAddr
  out.bits.csrData := MuxLookup1H(cur.zicsr(1, 0))(Seq(
    "b01".U(2.W) -> csrOpnd,
    "b10".U(2.W) -> (cur.csrSrc | csrOpnd),
    "b11".U(2.W) -> (cur.csrSrc & ~csrOpnd),
  ))
  out.bits.csrWen := zicsr && (out.bits.csrData === cur.csrSrc)

  override def outCond = cur.trap || Mux(cur.mul, Mux(mulFuncMul, mulValValid, divValValid), true.B)

  out.bits.rd := cur.rd
  out.bits.rdVal := Mux1H(Seq(
    (cur.mul && mulFuncMul) -> mulVal,
    (cur.mul && !mulFuncMul) -> divVal,
    zicsr -> cur.csrSrc,
    (!cur.mul && !zicsr) -> aluRes,
  ))
  out.bits.fwReady := cur.fwReady || cur.mul || zicsr
  out.bits.mem := cur.mem
  out.bits.amoFunc := cur.amoFunc
  out.bits.ret := cur.ret
  out.bits.fenceI := cur.fenceI
  out.bits.fenceVMA := cur.fenceVMA
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap
  out.bits.cause := cur.cause
}
