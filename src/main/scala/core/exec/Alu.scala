package core.exec

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

object Alu {
  val funcTruthTable = TruthTable(
    Map(
      BitPat("b000") -> BitPat("b00000001"),
      BitPat("b001") -> BitPat("b00000010"),
      BitPat("b010") -> BitPat("b00000100"),
      BitPat("b011") -> BitPat("b00001000"),
      BitPat("b100") -> BitPat("b00010000"),
      BitPat("b101") -> BitPat("b00100000"),
      BitPat("b110") -> BitPat("b01000000"),
      BitPat("b111") -> BitPat("b10000000")
    ), BitPat.dontCare(8)
  )
}

class Alu extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val func = Input(UInt(3.W))
    val sign = Input(Bool())
    val res = Output(UInt(32.W))
  })

  val func1H = decoder(io.func, Alu.funcTruthTable)
  val add = func1H(0)
  val shl = func1H(1)
  val lts = func1H(2)
  val ltu = func1H(3)
  val xor = func1H(4)
  val shr = func1H(5)
  val or  = func1H(6)
  val and = func1H(7)

  val adderEn = add || lts || ltu
  // 是否做减法，是减法需要将alu_b取反，在加法器输入进位
  val sub = io.sign || lts || ltu

  val a = io.a
  val b = io.b ^ Fill(32, sub)

  val shift = io.b(4, 0)

  val adderRes = (a +& b) + sub.asUInt

  val e = Mux1H(Seq(
    adderEn -> adderRes(31, 0),
    shl -> (a << shift)(31, 0),
    xor -> (a ^ b),
    shr -> Mux(io.sign, (a.asSInt >> shift).asUInt, a >> shift),
    or  -> (a | b),
    and -> (a & b),
  ))

  val cf = Mux(adderEn, adderRes(32), false.B)
  val sf = e(31)
  val of = (a(31) === b(31)) && (sf ^ a(31))

  io.res := Mux1H(Seq(
    lts -> Cat(0.U(31.W), sf ^ of),
    ltu -> Cat(0.U(31.W), !cf),
    !(lts || ltu) -> e
  ))
}
