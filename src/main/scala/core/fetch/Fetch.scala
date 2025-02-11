package core.fetch

import chisel3._
import chisel3.util._
import core.misc.{CacheWayFactory, MemReadIO}
import core.mmu.MmuIO
import utils.Config._

class FetchOut extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)

  val trap = Bool()
  // val cause = UInt(4.W) // only 12 (page fault)
}

class Fetch extends Module {
  val icacheFactory = new CacheWayFactory()
  import icacheFactory._

  val out = IO(Decoupled(new FetchOut))
  val memReadIO = IO(new MemReadIO)
  val mmuIO = IO(new MmuIO)
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

  import Fetch.State._
  val state = RegInit(stIdle)
  val idle = state === stIdle
  val mmuIng = state === stMmu
  val fetchIng = state === stFetch

  val genValid = RegInit(false.B)
  val genData = Reg(dataType)
  val genPf = Reg(Bool())

  val pc = RegInit(resetVec)
  val valid = RegInit(false.B)
  val ready = (!valid && idle) || (out.ready && out.valid)

  // ---------- Cache ----------
  when (io.flush) {
    pc := io.dnpc
  } .elsewhen (ready) {
    pc := pc + 4.U
  }
  icache.readIO.index := getIndex(pc)

  // ---------- Fetch ----------
  when (ready && idle) {
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
  val hit = Wire(Bool())
  when (genValid) { // gen前递（优先级高于ICache）
    hit := true.B
    out.bits.inst := genData(pcOffset)
    out.bits.trap := genPf
  } .elsewhen(cacheValid && cacheTag === pcTag) { // ICache命中
    hit := true.B
    out.bits.inst := cacheData(pcOffset)
    out.bits.trap := false.B
  } .otherwise { // miss
    hit := false.B
    out.bits.inst := DontCare
    out.bits.trap := false.B
  }
  out.valid := valid && !io.flush && hit
  out.bits.pc := cachePc

  // ---------- gen ----------
  val ppn = Reg(UInt(20.W))
  val cachePaddr = Cat(ppn, cachePc(11, 0))
  val burstOffset = Reg(UInt((offsetW - 2).W))
  val mmuPf = mmuIO.pf || (mmuIO.ppn(19, 16) =/= "h8".U(4.W))
  when (idle && valid && !hit && !io.flush) {
    state := stMmu
    burstOffset := 0.U
  }
  when (mmuIng && mmuIO.hit) {
    ppn := mmuIO.ppn
    genPf := mmuPf
    when (mmuPf) {
      state := stIdle
      genValid := true.B
    } .otherwise {
      state := stFetch
    }
  }
  when (fetchIng && memReadIO.resp) {
    genData(burstOffset) := memReadIO.data
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) {
      state := stIdle
      genValid := true.B
    }
  }
  mmuIO.valid := mmuIng
  mmuIO.fetch := true.B
  mmuIO.load := false.B
  mmuIO.store := false.B
  mmuIO.vpn := cachePc(31, 12)
  memReadIO.req := fetchIng
  memReadIO.addr := Cat(cachePaddr(31, offsetW), 0.U(offsetW.W))
  memReadIO.setBurst(blockN)
  icache.writeIO.en := valid && genValid && !genPf
  icache.writeIO.index := pcIndex
  icache.writeIO.tag := pcTag
  icache.writeIO.data := genData

  // ---------- Flush ----------
  when (io.flush) {
    valid := false.B
  }

  // TODO: 分支预测
}

object Fetch {
  object State extends ChiselEnum {
    val stIdle, stMmu, stFetch = Value
  }
}
