package core.csr

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}
import utils.BitPat1H

class CsrRegFile {
  val mstatus = RegInit(0.U(32.W))
  val mtvec = Reg(UInt(32.W))
  val mepc = Reg(UInt(32.W))
  val mcause = RegInit(0.U(32.W))
  val mtval = Reg(UInt(32.W))
  val mie = RegInit(0.U(32.W))
  val mip = RegInit(0.U(32.W))
  val medeleg = RegInit(0.U(32.W))
  val medelegh = RegInit(0.U(32.W))
  val mideleg = RegInit(0.U(32.W))

  val satp = RegInit(0.U(32.W))
  val mscratch = Reg(UInt(32.W))
  val sscratch = Reg(UInt(32.W))
  val stvec = Reg(UInt(32.W))
  val sepc = Reg(UInt(32.W))
  val scause = Reg(UInt(32.W))
  val stval = Reg(UInt(32.W))

  val counter = RegInit(0.U(64.W))

  val readMap: Seq[(UInt, UInt)] = Seq(
    "h100".U(12.W) -> (mstatus & "h000c0122".U(32.W)), // sstatus
    "h104".U(12.W) -> (mie & "h222".U(32.W)), // sie
    "h105".U(12.W) -> stvec,
    "h106".U(12.W) -> 0.U(32.W), // scounteren
    "h140".U(12.W) -> sscratch,
    "h141".U(12.W) -> sepc,
    "h142".U(12.W) -> scause,
    "h143".U(12.W) -> stval,
    "h144".U(12.W) -> (mip & "h222".U(32.W)), // sip
    "h180".U(12.W) -> satp,

    "h300".U(12.W) -> mstatus,
    "h301".U(12.W) -> "h40141111".U(32.W), // misa: RV32 IE MA SU
    "h302".U(12.W) -> medeleg,
    "h303".U(12.W) -> mideleg,
    "h304".U(12.W) -> mie,
    "h305".U(12.W) -> mtvec,
    "h310".U(12.W) -> 0.U(32.W), // mstatush
    "h312".U(12.W) -> medelegh,
    "h340".U(12.W) -> mscratch,
    "h341".U(12.W) -> mepc,
    "h342".U(12.W) -> mcause,
    "h343".U(12.W) -> mtval,
    "h344".U(12.W) -> mip,

    "hc00".U(12.W) -> counter(31, 0), // cycle
    "hc01".U(12.W) -> counter(31, 0), // time
    "hc02".U(12.W) -> counter(31, 0), // instret
    "hc80".U(12.W) -> counter(63, 32), // cycleh
    "hc81".U(12.W) -> counter(63, 32), // timeh
    "hc82".U(12.W) -> counter(63, 32), // instreth

    "hf11".U(12.W) -> "h79737978".U(32.W), // mvendorid
    "hf12".U(12.W) -> "h15fdeeb".U(32.W), // marchid
    "hf13".U(12.W) -> 0.U(32.W), // mimpid
    "hf14".U(12.W) -> 0.U(32.W), // mhartid
    "hf15".U(12.W) -> 0.U(32.W), // mconfigptr
  )

  def getCsrSrc(addr: UInt): (UInt, Bool) = {
    val n = readMap.length
    val t = TruthTable(
      readMap.indices.zip(readMap).map{ case (i, (k, _)) =>
        BitPat(k) -> BitPat1H(n + 1, i)
      }, BitPat.Y(1) ## BitPat.dontCare(n)
    )
    val res = decoder(addr, t)
    (Mux1H(res(n - 1, 0), readMap.map(_._2)), res(n))
  }
}
