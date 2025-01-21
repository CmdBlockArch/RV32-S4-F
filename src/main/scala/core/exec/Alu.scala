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

  class FuncBundle extends Bundle {
    val sign = Bool()
    val adderEn = Bool()
    val sub = Bool()
    val outE = Bool()

    val add = Bool()
    val shl = Bool()
    val lts = Bool()
    val ltu = Bool()
    val xor = Bool()
    val shr = Bool()
    val or  = Bool()
    val and = Bool()
  }

  def decodeFunc(func3: UInt, sign: Bool): FuncBundle = {
    val func1H = decoder(func3, funcTruthTable)
    val add = func1H(0)
    val lts = func1H(2)
    val ltu = func1H(3)

    val funcBundle = Wire(new FuncBundle)
    funcBundle.sign := sign
    funcBundle.adderEn := add || lts || ltu
    funcBundle.sub := sign || lts || ltu // 是否做减法，是减法需要将alu_b取反，在加法器输入进位
    funcBundle.outE := !(lts || ltu)

    funcBundle.add := func1H(0)
    funcBundle.shl := func1H(1)
    funcBundle.lts := func1H(2)
    funcBundle.ltu := func1H(3)
    funcBundle.xor := func1H(4)
    funcBundle.shr := func1H(5)
    funcBundle.or  := func1H(6)
    funcBundle.and := func1H(7)

    funcBundle
  }
}

class Alu extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val res = Output(UInt(32.W))
    val jmp = Output(Bool())
  })

  val func = IO(Input(new Alu.FuncBundle))

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
    func.add -> !brNe, // eq
    func.shl -> brNe, // ne
    func.xor -> brLts, // lt
    func.shr -> !brLts, // ge
    func.or  -> !brGeu, // ltu
    func.and -> brGeu // geu
  ))

}
