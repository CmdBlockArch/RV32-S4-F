package core.fetch

import chisel3._
import chisel3.util._

class BrPredFactory(val btbW: Int = 5, val rasW: Int = 3, val popW: Int = 4) {
  // popW <= btbW
  val btbN = 1 << btbW
  val rasN = 1 << rasW
  val popN = 1 << popW

  class PredIO extends Bundle {
    val pc = Output(UInt(32.W))

    val hit = Input(Bool())
    val index = Input(UInt(btbW.W))
    val jmp = Input(Bool()) // 若为高，则跳转到dnpc，否则+4
    val dnpc = Input(UInt(32.W))
  }

  class TrainIO extends Bundle {
    val en = Output(Bool())

    val pc = Output(UInt(32.W))
    val btb = Output(Bool()) // BR JMP CALL
    val push = Output(Bool()) // CALL COS
    val pop = Output(Bool()) // RET COS

    val hit = Output(Bool()) // pc是否已经存在于btb中，若为高则index有效
    val index = Output(UInt(btbW.W))

    val jmp = Output(Bool()) // 实际是否跳转
    val dnpc = Output(UInt(32.W)) // 跳转地址，此处认为BR始终跳转
    val snpc = Output(UInt(32.W))
  }

  class BtbReg extends Bundle {
    val valid = Bool()

    val cnt = UInt(2.W)
    def cntInc = Mux(cnt === 3.U, cnt, cnt + 1.U)
    def cntDec = Mux(cnt === 0.U, cnt, cnt - 1.U)

    val pcH = UInt(30.W)
    def pc = Cat(pcH, 0.U(2.W))

    val dnpcH = UInt(30.W)
    def dnpc = Cat(dnpcH, 0.U(2.W))

    def neg = valid && cnt === 0.U(2.W)
  }

  class BrPred extends Module {
    // BTB
    val btb = RegInit(VecInit(Seq.fill(btbN){
      val t = Wire(new BtbReg)
      t := DontCare
      t.valid := false.B
      t
    }))
    val btbFull = btb.map(_.valid).reduce(_ && _)
    val btbFreeIdx = btb.indexWhere(!_.valid)
    val btbHaveNeg = btb.map(t => t.neg).reduce(_ || _)
    val btbNegIdx = btb.indexWhere(t => t.neg)
    val btbRandIdx = Reg(UInt(btbW.W)); btbRandIdx := btbRandIdx + 1.U
    val btbEvictIdx = Mux(btbFull, Mux(btbHaveNeg, btbNegIdx, btbRandIdx), btbFreeIdx)

    // RAS
    val ras = Reg(Vec(rasN, UInt(30.W)))
    val rasPtr = Reg(UInt(rasW.W))
    val rasPtrInc = WireDefault(rasPtr + 1.U)
    val rasPtrDec = WireDefault(rasPtr - 1.U)
    val rasTop = ras(rasPtr)

    // POP表
    val pop = RegInit(VecInit(Seq.fill(popN){
      val t = Wire(new Bundle {
        val valid = Bool()
        val pcH = UInt(30.W)
      })
      t := DontCare
      t.valid := false.B
      t
    }))
    val popFull = pop.map(_.valid).reduce(_ && _)
    val popFreeIdx = pop.indexWhere(!_.valid)
    val popRandIdx = Reg(UInt(popW.W)); popRandIdx := popRandIdx + 1.U
    val popEvictIdx = Mux(popFull, popRandIdx, popFreeIdx)

    // ---------- Pred ----------
    val predIO = IO(Flipped(new PredIO))
    val predPc = predIO.pc

    val btbHitVec = VecInit(btb.map(i => i.valid && i.pc === predPc))
    val btbHit = btbHitVec.reduce(_ || _)
    val btbHitIdx = btbHitVec.indexWhere(i => i)
    val btbLine = Mux1H(btbHitVec, btb)
    val btbCnt = btbLine.cnt
    val btbDnpc = btbLine.dnpc

    val popHitVec = VecInit(pop.map(i => i.valid && i.pcH === predPc(31, 2)))
    val popHit = popHitVec.reduce(_ || _) // popHit为高时，btbHit必为低
    val popHitIdx = popHitVec.indexWhere(i => i)

    predIO.hit := btbHit || popHit
    predIO.index := Mux(popHit, popHitIdx, btbHitIdx)
    predIO.jmp := popHit || btbCnt(1)
    predIO.dnpc := Mux(popHit, rasTop, btbDnpc)

    // ---------- Train ----------
    val trainIO = IO(Flipped(new TrainIO))
    val trHit = trainIO.hit
    val trPcH = trainIO.pc(31, 2)
    val trDnpcH = trainIO.dnpc(31, 2)
    val trSnpcH = trainIO.snpc(31, 2)
    val trIdx = trainIO.index
    val trJmp = trainIO.jmp
    val btbTrHit = trHit && btb(trIdx).pcH === trPcH
    val popTrHit = trHit && pop(trIdx(popW - 1, 0)).pcH === trPcH
    when (trainIO.en) {
      when (trainIO.btb) {
        when (btbTrHit) {
          btb(trIdx).cnt := Mux(trJmp, btb(trIdx).cntInc, btb(trIdx).cntDec)
          btb(trIdx).dnpcH := trDnpcH
        } .elsewhen (trJmp) {
          btb(btbEvictIdx).valid := true.B
          btb(btbEvictIdx).cnt := "b10".U(2.W)
          btb(btbEvictIdx).pcH := trPcH
          btb(btbEvictIdx).dnpcH := trDnpcH
        }
      }
      when (trainIO.pop && !popTrHit) {
        pop(popEvictIdx).valid := true.B
        pop(popEvictIdx).pcH := trPcH
      }
      when (trainIO.push && trainIO.pop) {
        ras(rasPtr) := trSnpcH
      } .elsewhen (trainIO.push) {
        ras(rasPtrInc) := trSnpcH
        rasPtr := rasPtrInc
      } .elsewhen (trainIO.pop) {
        rasPtr := rasPtrDec
      }
    }

    // flush
    val io = IO(new Bundle {
      val flush = Input(Bool())
    })
    when (io.flush) {
      btb.foreach(_.valid := false.B)
      pop.foreach(_.valid := false.B)
    }
  }
}
