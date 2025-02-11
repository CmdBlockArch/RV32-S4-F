package core.mmu

import chisel3._
import chisel3.util._

class Tlb(val indexW: Int = 4) extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
  })

  val lineN = 1 << indexW
  val lines = RegInit(VecInit(Seq.fill(lineN)({
    val init = Wire(MmuBundle.tlbReg)
    init := DontCare
    init.valid := false.B
    init
  })))

  val readIO = IO(Flipped(MmuBundle.tlbRead))
  val isLineHit: MmuBundle => Bool = i => {
    i.valid && i.vpn1 === readIO.vpn1 && (i.mega || i.vpn0 === readIO.vpn0)
  }
  val hitSel = lines.map(isLineHit)
  readIO.value := Mux1H(hitSel, lines).value
  readIO.valid := hitSel.reduce(_ || _)

  val randomIndex = Reg(UInt(indexW.W)); randomIndex := randomIndex + 1.U
  val freeIndex = lines.indexWhere(!_.valid)
  val full = lines.map(_.valid).reduce(_ && _)
  val evictIndex = Mux(full, randomIndex, freeIndex)

  val writeIO = IO(Flipped(MmuBundle.tlbWrite))
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
