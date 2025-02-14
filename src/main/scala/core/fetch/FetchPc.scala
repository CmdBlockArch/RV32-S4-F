package core.fetch

import chisel3._
import chisel3.util._
import InstCache.{icacheFactory => ic}
import Bpu.{factory => bpu}
import utils.Config._

class FetchPreOut extends Bundle {
  val pc = Output(UInt(32.W))

  val icValid = Output(ic.validType)
  val icTag = Output(ic.tagType)
  val icData = Output(ic.dataType)

  val bpuHit = Output(Bool())
  val bpuIdx = Output(UInt(bpu.btbW.W))
  val dnpc = Output(UInt(32.W))
}

class FetchPc extends Module {
  val out = IO(Decoupled(new FetchPreOut))
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val dnpc = Input(UInt(32.W))
  })

  // pc
  val pc = RegInit(resetVec)
  out.valid := true.B
  out.bits.pc := pc

  // ICache读
  val icReadIO = IO(new ic.ReadIO)
  icReadIO.index := ic.getIndex(pc)
  out.bits.icValid := icReadIO.valid
  out.bits.icTag := icReadIO.tag
  out.bits.icData := icReadIO.data

  // 分支预测
  val bpuPredIO = IO(new bpu.PredIO)
  bpuPredIO.pc := pc
  val bpuPredValid = bpuPredIO.hit && bpuPredIO.jmp
  val predPc = Mux(bpuPredValid, bpuPredIO.dnpc, pc + 4.U)
  when (io.flush) { pc := io.dnpc }
    .elsewhen (out.ready) { pc := predPc }
  out.bits.bpuHit := bpuPredIO.hit
  out.bits.bpuIdx := bpuPredIO.index
  out.bits.dnpc := predPc
}
