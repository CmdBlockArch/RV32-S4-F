package core.exec

import chisel3._
import chisel3.util._
import core.decode.FuncBundle

class Alu extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val res = Output(UInt(32.W))
    val jmp = Output(Bool())
  })

  val func = IO(Input(new FuncBundle))
  val brFunc = func.brFunc

  val a = io.a
  val b = io.b ^ Fill(32, func.sub)

  val shift = io.b(4, 0)

  val adderRes = (a +& b) + func.sub.asUInt
  val xorRes = a ^ io.b

  val e = Mux1H(Seq(
    func.adderEn -> adderRes(31, 0),
    func.shl -> (a << shift)(31, 0),
    func.xor -> xorRes,
    func.shr -> Mux(func.sign, (a.asSInt >> shift).asUInt, a >> shift),
    func.or  -> (a | io.b),
    func.and -> (a & io.b),
  ))

  val cf = Mux(func.adderEn, adderRes(32), false.B)
  val sf = e(31)
  val of = (a(31) === b(31)) && (sf ^ a(31))

  io.res := Mux1H(Seq(
    func.lts -> Cat(0.U(31.W), sf ^ of),
    func.ltu -> Cat(0.U(31.W), !cf),
    func.outE -> e
  ))

  val brCF = adderRes(32)
  val brSF = adderRes(31)
  val brOF = (a(31) === b(31)) && (brSF ^ a(31))

  val brNe = xorRes.orR
  val brLts = brSF ^ brOF
  val brGeu = brCF

  io.jmp := Mux1H(Seq(
    brFunc.eq -> !brNe,
    brFunc.ne -> brNe,
    brFunc.lt -> brLts,
    brFunc.ge -> !brLts,
    brFunc.ltu  -> !brGeu,
    brFunc.geu -> brGeu
  ))

}
