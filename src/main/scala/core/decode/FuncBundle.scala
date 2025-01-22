package core.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

object FuncBundle {
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

  def brFunc = {
    val f = Wire(new Bundle {
      val eq = Bool()
      val ne = Bool()
      val lt = Bool()
      val ge = Bool()
      val ltu = Bool()
      val geu = Bool()
    })
    f.eq := add
    f.ne := shl
    f.lt := xor
    f.ge := shr
    f.ltu := or
    f.geu := and
    f
  }

  def mulFunc = {
    val f = Wire(new Bundle {
      val mul = Bool()
      val mulh = Bool()
      val mulhsu = Bool()
      val mulhu = Bool()
      val div = Bool()
      val divu = Bool()
      val rem = Bool()
      val remu = Bool()
    })
    f.mul := add
    f.mulh := shl
    f.mulhsu := lts
    f.mulhu := ltu
    f.div := xor
    f.divu := shr
    f.rem := or
    f.remu := and
    f
  }

  def csrFunc = {
    val f = Wire(new Bundle {
      val opnd = Bool()
      val rw = Bool()
      val rs = Bool()
      val rc = Bool()
    })
    f.opnd := shr || or || and
    f.rw := shl || shr
    f.rs := lts || or
    f.rc := ltu || and
    f
  }
}
