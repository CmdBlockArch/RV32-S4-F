package core.fetch

import chisel3._
import chisel3.util._
import core.misc.MemReadIO
import core.cache.CacheWayFactory

class FetchOut extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)

  val trap = Bool()
  val cause = UInt(4.W)
}

class Fetch extends Module {
  val icacheFactory = new CacheWayFactory()
  import icacheFactory._

  val out = IO(Decoupled(new FetchOut))
  val memReadIO = IO(new MemReadIO)
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val dnpc = Input(UInt(32.W))

    val fenceI = Input(Bool())
  })

  val icache = Module(new icacheFactory.CacheWay)
  icache.io.flush := io.fenceI

  val cachePc = Reg(UInt(32.W))
  val cacheValid = Reg(Bool())
  val cacheTag = Reg(UInt(tagW.W))
  val cacheData = Reg(dataType)

  val req = RegInit(false.B)
  val genValid = RegInit(false.B)
  val genData = Reg(dataType)
  val genError = Reg(Bool())

  val pc = RegInit(0x80000000L.U(32.W))
  val valid = RegInit(false.B)
  val ready = (!valid && !req) || (out.ready && out.valid)

  // ---------- Cache ----------
  when (io.flush) {
    pc := io.dnpc
  } .elsewhen (ready) {
    pc := pc + 4.U
  }
  icache.readIO.index := getIndex(pc)

  // ---------- Fetch ----------
  when (ready && !req) {
    valid := !io.flush
    cachePc := pc
    cacheValid := icache.readIO.valid
    cacheTag := icache.readIO.tag
    cacheData := icache.readIO.data
    genValid := false.B
  }
  val pcTag = getTag(cachePc)
  val pcIndex = getIndex(cachePc)
  val pcOffset = getOffset(cachePc)
  val pcMisaligned = cachePc(1, 0).orR
  val hit = Wire(Bool())
  when (genValid) { // gen前递（优先级高于ICache）
    hit := true.B
    out.bits.inst := genData(pcOffset)
    out.bits.trap := genError || pcMisaligned
    out.bits.cause := genError
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
  out.valid := valid && !io.flush && hit
  out.bits.pc := cachePc

  // ---------- gen ----------
  val burstOffset = Reg(UInt((offsetW - 2).W))
  memReadIO.req := req
  memReadIO.addr := Cat(getTag(cachePc), getIndex(cachePc), 0.U(offsetW.W))
  memReadIO.setBurst(blockN)
  when (!req && valid && !hit && !io.flush) {
    req := true.B
    burstOffset := 0.U
  }
  when (req && memReadIO.resp) {
    genData(burstOffset) := memReadIO.data
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) {
      req := false.B
      genValid := true.B
      genError := memReadIO.err
    }
  }
  icache.writeIO.en := valid && genValid && !genError
  icache.writeIO.dirty := DontCare
  icache.writeIO.index := pcIndex
  icache.writeIO.tag := pcTag
  icache.writeIO.data := genData

  // ---------- Flush ----------
  when (io.flush) {
    valid := false.B
  }

  // TODO: 分支预测
}
