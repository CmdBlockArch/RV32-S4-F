package core.mem

import chisel3._
import chisel3.util._
import utils.PiplineModule
import core.gpr.GprFwIO
import core.misc.{MemReadIO, MemWriteIO, MemBurstWriteHelper}

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

  def wbFlush = csrWen || ret.orR || fenceI || fenceVMA || trap
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

  // 命中判断逻辑
  val genHit = WireDefault(genValid && genIndex === index && genTag === tag)
  val dcacheHit = WireDefault(cur.dcacheValid && cur.dcacheTag === tag)
  val hit = WireDefault(genHit || dcacheHit)

  // 状态机
  val evictReq = RegInit(false.B)
  val readReq = RegInit(false.B)
  val reqFin = RegInit(false.B)
  val idle = !(evictReq || readReq || reqFin)
  val burstOffset = Reg(UInt((dc.offsetW - 2).W))
  when (idle) {
    when (valid && mem && !hit) { // valid && mem && !hit && !cur.trap
      when (cur.dcacheDirty) {
        evictReq := true.B
      } .otherwise {
        readReq := true.B
        burstOffset := 0.U
      }
    }
  }
  when (memBwIO.resp) {
    evictReq := false.B
    readReq := true.B
    burstOffset := 0.U
  }
  when (memReadIO.resp) {
    genData(burstOffset) := memReadIO.data
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) {
      readReq := false.B
      reqFin := true.B
    }
  }
  when (reqFin) {
    reqFin := false.B // wb不可能阻塞，保持一周期即可
  }
  memBwIO.req := evictReq
  memBwIO.addr := Cat(cur.dcacheTag, index, 0.U(dc.offsetW.W))
  memBwIO.data := cur.dcacheData
  memReadIO.req := readReq
  memReadIO.addr := Cat(addr(31, dc.offsetW), 0.U(dc.offsetW.W))
  memReadIO.setBurst(dc.blockN)

  // load/store结果
  val hitData = Mux(genHit, genData, cur.dcacheData)
  val data = Mux(reqFin, genData(offset), hitData(offset))
  val loadVal = Mem.getLoadVal(cur.mem, addr, data)
  val storeVal = Mem.getStoreVal(cur.mem, addr, data, cur.data)

  // dcache写入
  when (valid && store && dcacheHit && !genHit) {
    genData := cur.dcacheData
  }
  when ((valid && store && hit) || reqFin) {
    genValid := true.B
    genDirty := !reqFin
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
  gprFwIO.ready := cur.fwReady || (load && hit)
  gprFwIO.fwVal := Mux(load, loadVal, cur.rdVal)

  // 阶段完成条件
  setOutCond(cur.trap || !mem || hit || reqFin)

  // 输出
  out.bits.rd := cur.rd
  out.bits.rdVal := Mux(load, loadVal, cur.rdVal)
  out.bits.csrWen := cur.csrWen
  out.bits.csrAddr := cur.csrAddr
  out.bits.csrData := cur.data
  out.bits.ret := cur.ret
  out.bits.fenceI := cur.fenceI
  out.bits.fenceVMA := cur.fenceVMA
  out.bits.pc := cur.pc
  out.bits.trap := cur.trap
  out.bits.cause := cur.cause

  // TODO: LR SC AMO
}

object Mem {
  def getStoreVal(mem: UInt, addr: UInt, old: UInt, data: UInt): UInt = {
    val offset = Cat(addr(1, 0), 0.U(3.W))
    val wstrb = Wire(UInt(32.W))
    wstrb := Cat(Fill(16, mem(1)), Fill(8, mem(1, 0).orR), 1.U(8.W)) << offset
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
