package core.mmu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

class TlbValue extends Bundle {
  val valid = Bool()

  // 4MB大页
  val mega = Bool()
  // 权限位
  val read = Bool()
  val write = Bool()
  val exec = Bool()
  val user = Bool()
  // 脏位
  val dirty = Bool()
  // 物理页号
  val ppn = UInt(20.W)

  def value = this.viewAsSupertype(new TlbValue)
}

trait TlbVpn {
  // 虚拟页号
  val vpn = Flipped(UInt(20.W))
  def vpn1 = vpn(19, 10)
  def vpn0 = vpn(9, 0)
}

class TlbBundle extends TlbValue with TlbVpn

class Tlb(val indexW: Int = 4) extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
  })

  val lineN = 1 << indexW
  val lines = RegInit(VecInit(Seq.fill(lineN)({
    val init = Wire(Output(new TlbBundle))
    init := DontCare
    init.valid := false.B
    init
  })))

  val readIO = IO(new TlbBundle)
  val isLineHit: TlbBundle => Bool = i => {
    i.valid && i.vpn1 === readIO.vpn1 && (i.mega || i.vpn0 === readIO.vpn0)
  }
  val hitSel = lines.map(isLineHit)
  readIO.value := Mux1H(hitSel, lines).value
  readIO.valid := hitSel.reduce(_ || _)

  val writeIO = IO(Input(new TlbBundle))
  val evictIndex = Reg(UInt(indexW.W)); evictIndex := evictIndex + 1.U
  (0 until lineN).zip(lines).foreach { case (i, line) =>
    when (writeIO.valid && i.U === evictIndex) {
      line.value := writeIO.value
      line.valid := true.B
      line.vpn := writeIO.vpn
    }
  }

  when (io.flush) {
    lines.foreach(_.valid := false.B)
  }
}
