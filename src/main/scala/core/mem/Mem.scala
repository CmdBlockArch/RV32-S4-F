package core.mem

import chisel3._
import chisel3.util._
import utils.PiplineModule
import core.gpr.GprFwIO
import core.misc.{MemReadIO, MemWriteIO, MemBurstWriteHelper}
import utils.Config._

import DataCache.{dcacheFactory => dc}

class MemOut extends Bundle {
  val rd = Output(UInt(5.W))
  val rdVal = Output(UInt(32.W))
  // CSR
  val csrWen = Output(Bool())
  val csrAddr = Output(UInt(12.W))
  val csrData = Output(UInt(32.W))
  // SYS
  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())
  // 异常
  val pc = Output(UInt(32.W))
  val trap = Output(Bool())
  val cause = Output(UInt(4.W))
  // 调试
  val inst = if (debug) Some(Output(UInt(32.W))) else None
  val dnpc = if (debug) Some(Output(UInt(32.W))) else None
  val skip = if (debug) Some(Output(Bool())) else None

  def wbFlush = csrWen || ret.orR || fenceI || fenceVMA || trap
}

object Mem {
  object State extends ChiselEnum {
    val stIdle, stWrite, stRead, stHold = Value
  }
  def getStoreVal(mem: UInt, addr: UInt, old: UInt, data: UInt): UInt = {
    val offset = Cat(addr(1, 0), 0.U(3.W))
    val wstrb = Wire(UInt(32.W))
    wstrb := Cat(Fill(16, mem(1)), Fill(8, mem(1, 0).orR), Fill(8, 1.U(1.W))) << offset
    val wdata = Wire(UInt(32.W)); wdata := data << offset
    (old & ~wstrb) | (wdata & wstrb)
  }
  def getLoadVal(mem: UInt, addr: UInt, old: UInt): UInt = {
    val offset = Cat(addr(1, 0), 0.U(3.W))
    val rdata = Wire(UInt(32.W)); rdata := old >> offset
    Mux(mem(1), rdata, Mux(mem(0),
      Cat(Fill(16, rdata(15) && mem(2)), rdata(15, 0)), // 01
      Cat(Fill(24, rdata( 7) && mem(2)), rdata( 7, 0)), // 00
    ))
  }
}

class Mem extends PiplineModule(new MemPreOut, new MemOut) {
  // 内存读写端口
  val memWriteIO = IO(new MemWriteIO)
  val memBw = Module((new MemBurstWriteHelper(dc.blockN))())
  memWriteIO :<>= memBw.slave
  val memBwIO = memBw.master
  val memReadIO = IO(new MemReadIO)

  // 内存操作控制信号
  val mem = cur.mem.orR
  val load = cur.mem(3)
  val store = cur.mem(3, 2) === "b01".U(2.W)

  // cache写入生成
  val genValid = RegInit(false.B)
  val genDirty = Reg(Bool())
  val genTag = Reg(UInt(dc.tagW.W))
  val genIndex = Reg(UInt(dc.indexW.W))
  val genData = Reg(dc.dataType)
  val dcacheWriteIO = IO(new dc.writeIO)
  dcacheWriteIO.en := genValid
  dcacheWriteIO.dirty := genDirty
  dcacheWriteIO.tag := genTag
  dcacheWriteIO.index := genIndex
  dcacheWriteIO.data := genData

  // 当前指令读取/写入的地址
  val addr = cur.rdVal
  val tag = dc.getTag(addr)
  val index = dc.getIndex(addr)
  val offset = dc.getOffset(addr)

  // gen优先级更高
  val useGen = genValid && index === genIndex
  val dcValid = useGen || cur.dcacheValid
  val dcTag = Mux(useGen, genTag, cur.dcacheTag)
  val dcAddr = Cat(dcTag, index, 0.U(dc.offsetW.W))
  val dcDirty = Mux(useGen, genDirty, cur.dcacheDirty)
  val dcData = Mux(useGen, genData.asUInt, cur.dcacheData.asUInt).asTypeOf(dc.dataType)
  val dcHit = dcValid && dcTag === tag
  val dcEvict = dcValid && dcDirty

  // 状态机
  import Mem.State._
  val state = RegInit(stIdle)
  val burstOffset = Reg(UInt((dc.offsetW - 2).W))
  val idle = state === stIdle
  val hold = state === stHold
  when (idle && valid && mem && !dcHit && dcEvict) { state := stWrite }
  when ((idle && valid && mem && !dcHit && !dcEvict) || memBwIO.resp) {
    state := stRead
    genValid := false.B
    cur.dcacheValid := false.B
    burstOffset := 0.U
  }
  when (memReadIO.resp) {
    genData(burstOffset) := memReadIO.data
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) { state := stHold }
  }
  when (hold) { state := stIdle } // wb不可能阻塞，保持一周期即可
  memBwIO.req := state === stWrite
  memBwIO.addr := dcAddr
  memBwIO.data := dcData
  memReadIO.req := state === stRead
  memReadIO.addr := Cat(addr(31, dc.offsetW), 0.U(dc.offsetW.W))
  memReadIO.setBurst(dc.blockN)

  // load/store结果
  val data = Mux(hold, genData(offset).asUInt, dcData(offset).asUInt)
  val loadVal = Mem.getLoadVal(cur.mem, addr, data)
  val storeVal = Mem.getStoreVal(cur.mem, addr, data, cur.data)

  // dcache写入
  when (valid && store && dcHit && !useGen) {
    genData := cur.dcacheData
  }
  when ((valid && store && dcHit) || hold) {
    genValid := true.B
    genDirty := store
    genTag := tag
    genIndex := index
    when (store) {
      genData(offset) := storeVal
    }
  }

  // 前递
  val gprFwIO = IO(new GprFwIO)
  gprFwIO.valid := valid
  gprFwIO.rd := cur.rd
  gprFwIO.ready := cur.fwReady || (load && dcHit)
  val rdVal = Mux(load, loadVal, Mux(cur.sc, 0.U, cur.rdVal))
  gprFwIO.fwVal := rdVal

  // 阶段完成条件
  setOutCond(!mem || dcHit || hold)

  // 输出
  out.bits.rd := cur.rd
  out.bits.rdVal := rdVal
  // csrWen信号直通WB，保证指令进入WB同时写入CSR（和GPR一样）
  // 基于先验：CSR写入时无访存操作，mem信号必为0
  // TODO: 考察除了zicsr外的CSR写入
  out.bits.csrWen := valid && !flush && cur.csrWen
  out.bits.csrAddr := cur.csrAddr
  out.bits.csrData := cur.data
  out.bits.ret := cur.ret
  out.bits.fenceI := cur.fenceI
  out.bits.fenceVMA := cur.fenceVMA
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap
  out.bits.cause := cur.cause

  // TODO: LR SC AMO

  if (debug) {
    out.bits.inst.get := cur.inst.get
    out.bits.dnpc.get := cur.dnpc.get
    out.bits.skip.get := cur.skip.get
  }
}
