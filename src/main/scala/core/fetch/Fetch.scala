package core.fetch

import chisel3._
import chisel3.util._
import core.misc.MemReadIO
import core.mmu.MmuIO
import InstCache.{icacheFactory => ic}
import Bpu.{factory => bpu}

class FetchOut extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)

  val bpuHit = Bool()
  val bpuIdx = UInt(bpu.btbW.W)
  val dnpc = UInt(32.W)

  val trap = Bool()
  // val cause = UInt(4.W) // only 12 (page fault)
}

class Fetch extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
  })
  val out = IO(Decoupled(new FetchOut))
  val memReadIO = IO(new MemReadIO)
  val mmuIO = IO(new MmuIO)

  // 状态机状态
  import Fetch.State._
  val state = RegInit(stIdle)
  val idle = state(1)
  val mmuIng = state === stMmu
  val fetchIng = state === stFetch

  // 接收FetchPre输出
  val valid = RegInit(false.B)
  val cur = Reg(new FetchPreOut)
  val in = IO(Flipped(Decoupled(new FetchPreOut)))
  in.ready := (!valid && idle) || (out.ready && out.valid)
  when (in.ready && idle) {
    valid := !io.flush
    cur := in.bits
  }
  val tag = ic.getTag(cur.pc)
  val index = ic.getIndex(cur.pc)
  val offset = ic.getOffset(cur.pc)

  // ICache写入生成
  val genValid = RegInit(false.B)
  val genWay = Reg(UInt(ic.wayW.W))
  val genData = Reg(ic.wayDataType)
  val genPf = Reg(Bool())
  val icacheWriteIO = IO(new ic.WriteIO)
  icacheWriteIO.valid := valid && genValid && !genPf
  icacheWriteIO.index := index
  icacheWriteIO.way := genWay
  icacheWriteIO.tag := tag
  icacheWriteIO.data := genData
  when (in.ready && idle) { genValid := false.B }

  // ICache命中 & 输出
  val hit = Wire(Bool())
  val curHitVec = ic.tagHitVec(cur.icValid, cur.icTag, tag)
  val curHitWay = curHitVec.onlyIndexWhere(_.asBool)
  when (genValid) {
    hit := true.B
    out.bits.inst := genData(offset)
    out.bits.trap := genPf
  } .otherwise {
    hit := curHitVec.reduce(_ || _)
    out.bits.inst := Mux1H(curHitVec, cur.icData)(offset)
    out.bits.trap := false.B
  }
  out.valid := valid && !io.flush && hit
  out.bits.pc := cur.pc
  out.bits.bpuHit := cur.bpuHit
  out.bits.bpuIdx := cur.bpuIdx
  out.bits.dnpc := cur.dnpc

  // 二路组相联 dcache PLRU
  val plru = Reg(ic.plruType)
  val evictWay = ~plru(index)
  when (valid && hit) {
    plru(index) := Mux(genValid, genWay, curHitWay)
  }

  // 状态机
  val ppn = Reg(UInt(20.W))
  val paddr = Cat(ppn, cur.pc(11, 0))
  val burstOffset = Reg(UInt((ic.offsetW - 2).W))
  val mmuPf = mmuIO.pf || (mmuIO.ppn(19, 16) =/= "h8".U(4.W))
  when (idle && valid && !hit && !io.flush) {
    state := stMmu
    burstOffset := 0.U
    genWay := evictWay
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
  mmuIO.vpn := cur.pc(31, 12)
  memReadIO.req := fetchIng
  memReadIO.addr := Cat(paddr(31, ic.offsetW), 0.U(ic.offsetW.W))
  memReadIO.setBurst(ic.blockN)

  // flush
  when (io.flush) { valid := false.B }
}

object Fetch {
  object State {
    val stIdle  = "b10".U
    val stMmu   = "b00".U
    val stFetch = "b01".U
  }
}
