package core.csr

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}
import utils.BitPat1H

class CsrReadPort(r: CsrRegFile) {
  val readMap: Seq[(UInt, UInt)] = Seq(
    "h100".U(12.W) -> // sstatus
      Cat(0.U(12.W), r.MXR, r.SUM, 0.U(9.W), r.SPP, 0.U(2.W), r.SPIE, 0.U(3.W), r.SIE, 0.B),
    "h104".U(12.W) -> Cat(0.U(22.W), r.SEIE, 0.U(3.W), r.STIE, 0.U(3.W), r.SSIE, 0.B), // sie
    "h105".U(12.W) -> r.stvec,
    "h106".U(12.W) -> 0.U(32.W), // scounteren
    "h140".U(12.W) -> r.sscratch,
    "h141".U(12.W) -> r.sepc,
    "h142".U(12.W) -> r.scause,
    "h143".U(12.W) -> r.stval,
    "h144".U(12.W) -> Cat(0.U(22.W), r.SEIP, 0.U(3.W), r.STIP, 0.U(3.W), r.SSIP, 0.B), // sip
    "h180".U(12.W) -> r.satp,

    "h300".U(12.W) -> // mstatus
      Cat(0.U(9.W), r.TSR, r.TW, r.TVM, r.MXR, r.SUM, r.MPRV, 0.U(4.W), r.MPP, 0.U(2.W), r.SPP,
        r.MPIE, 0.B, r.SPIE, 0.B, r.MIE, 0.B, r.SIE, 0.B),
    "h301".U(12.W) -> "h40141111".U(32.W), // misa: RV32 IE MA SU
    "h302".U(12.W) -> r.medeleg,
    "h303".U(12.W) -> r.mideleg,
    "h304".U(12.W) -> // mie
      Cat(0.U(20.W), r.MEIE, 0.B, r.SEIE, 0.B, r.MTIE, 0.B, r.STIE, 0.B, r.MSIE, 0.B, r.SSIE, 0.B),
    "h305".U(12.W) -> r.mtvec,
    "h310".U(12.W) -> 0.U(32.W), // mstatush
    "h312".U(12.W) -> r.medelegh,
    "h340".U(12.W) -> r.mscratch,
    "h341".U(12.W) -> r.mepc,
    "h342".U(12.W) -> r.mcause,
    "h343".U(12.W) -> r.mtval,
    "h344".U(12.W) -> // mip
      Cat(0.U(20.W), r.MEIP, 0.B, r.SEIP, 0.B, r.MTIP, 0.B, r.STIP, 0.B, r.MSIP, 0.B, r.SSIP, 0.B),

    "hc00".U(12.W) -> r.counter(31, 0), // cycle
    "hc01".U(12.W) -> r.counter(31, 0), // time
    "hc02".U(12.W) -> r.counter(31, 0), // instret
    "hc80".U(12.W) -> r.counter(63, 32), // cycleh
    "hc81".U(12.W) -> r.counter(63, 32), // timeh
    "hc82".U(12.W) -> r.counter(63, 32), // instreth

    "hf11".U(12.W) -> "h79737978".U(32.W), // mvendorid
    "hf12".U(12.W) -> "h15fdeeb".U(32.W), // marchid
    "hf13".U(12.W) -> 0.U(32.W), // mimpid
    "hf14".U(12.W) -> 0.U(32.W), // mhartid
    "hf15".U(12.W) -> 0.U(32.W), // mconfigptr
  )

  def apply(addr: UInt): (UInt, Bool) = {
    // TODO: 检查特权级
    val n = readMap.length
    val t = TruthTable(
      readMap.indices.zip(readMap).map{ case (i, (k, _)) =>
        BitPat(k) -> BitPat1H(n + 1, i)
      }, BitPat.Y(1) ## BitPat.dontCare(n)
    )
    val res = decoder(addr, t)
    val data = Mux1H(res(n - 1, 0), readMap.map(_._2))
    val err = res(n)
    (data, err)
  }
}
