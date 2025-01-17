package core.fetch

import chisel3._
import chisel3.util._
import core.misc.MemReadIO

class FetchOut extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)

  val trap = Bool()
  val cause = UInt(4.W)
}

class Fetch extends Module {
  import core.fetch.InstCacheUtils._

  val out = IO(Decoupled(new FetchOut))
  val memReadIO = IO(new MemReadIO)
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val fenceI = Input(Bool())
    val dnpc = Input(UInt(32.W))
  })

  val iCache = Module(new InstCacheWay)
  iCache.io.flush := io.fenceI

  val pc = RegInit(0x80000000L.U(32.W))
  val valid = RegInit(false.B)
  val ready = !valid || (out.ready && out.valid)

  val cachePc = Reg(UInt(32.W))
  val cacheValid = Reg(Bool())
  val cacheTag = Reg(UInt(tagW.W))
  val cacheData = Reg(dataType)

  val req = RegInit(false.B)
  val writeValid = RegInit(false.B)
  val writeData = Reg(dataType)
  val writeError = Reg(Bool())

  // ---------- Cache ----------
  when (io.flush) {
    pc := io.dnpc
  } .elsewhen (ready) {
    pc := pc + 4.U
  }
  iCache.readIO.index := getIndex(pc)

  // ---------- Fetch ----------
  when (ready && !req) {
    valid := !io.flush
    cachePc := pc
    cacheValid := iCache.readIO.valid
    cacheTag := iCache.readIO.tag
    cacheData := iCache.readIO.data
    writeValid := false.B
  }
  val pcTag = getTag(cachePc)
  val pcIndex = getIndex(cachePc)
  val pcOffset = getOffset(cachePc)
  val pcLow = cachePc(1, 0)
  val pcMisaligned = pcLow.orR
  val hit = Wire(Bool())
  when (writeValid) { // write前递（优先级高于ICache）
    hit := true.B
    out.bits.inst := writeData(pcOffset)
    out.bits.trap := writeError || pcMisaligned
    out.bits.cause := writeError
  } .elsewhen(cacheValid && cacheTag === pcTag) { // ICache命中
    hit := true.B
    out.bits.inst := cacheData(pcOffset)
    out.bits.trap := pcMisaligned
    out.bits.cause := 0.U
  } .otherwise { // miss
    hit := false.B
    out.bits.inst := DontCare
    out.bits.trap := DontCare
    out.bits.cause := DontCare
  }
  out.valid := valid && hit
  out.bits.pc := cachePc

  // ---------- Write ----------
  val burstOffset = Reg(UInt((offsetW - 2).W))
  memReadIO.req := req
  memReadIO.addr := cachePc
  memReadIO.size := "b10".U // 4 bytes
  memReadIO.burst := "b10".U // wrap burst
  memReadIO.len := (blockN - 1).U
  when (!req && valid && !hit && !io.flush) {
    req := true.B
    burstOffset := 0.U
  }
  when (req && memReadIO.resp) {
    writeData(burstOffset) := memReadIO.data
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) {
      req := false.B
      writeValid := true.B
      writeError := memReadIO.err
    }
  }
  iCache.writeIO.en := valid && writeValid && !writeError
  iCache.writeIO.index := pcIndex
  iCache.writeIO.tag := pcTag
  iCache.writeIO.data := writeData

  // ---------- Flush ----------
  when (io.flush) {
    valid := false.B
  }

  // TODO: 分支预测
}
