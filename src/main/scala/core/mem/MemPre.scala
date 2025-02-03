package core.mem

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import utils.PiplineModule
import core.exec.ExecOut
import core.gpr.GprFwIO
import DataCache.{dcacheFactory => dc}
import core.misc.{MemReadIO, MemWriteIO}
import core.mem.Mem
import utils.Config._

class MemPreOut extends ExecOut {
  // sc
  val sc = Output(Bool())
  // Data Cache
  val dcacheValid = Output(Bool())
  val dcacheDirty = Output(Bool())
  val dcacheTag = Output(UInt(dc.tagW.W))
  val dcacheData = Output(dc.dataType)
  // debug
  val skip = if (debug) Some(Output(Bool())) else None
}

object MemPre {
  object State extends ChiselEnum {
    val stIdle, stRead, stWrite, stHold = Value
  }
}

class MemPre extends PiplineModule(new ExecOut, new MemPreOut) {
  // output
  out.bits.viewAsSupertype(new ExecOut) := cur

  // dcache读
  val dcacheReadIO = IO(new dc.readIO)
  dcacheReadIO.index := dc.getIndex(cur.rdVal)
  out.bits.dcacheValid := dcacheReadIO.valid
  out.bits.dcacheDirty := dcacheReadIO.dirty
  out.bits.dcacheTag := dcacheReadIO.tag
  out.bits.dcacheData := dcacheReadIO.data

  // LR&SC
  val lrsc = !cur.mem(3, 2).orR && cur.mem(1, 0).xorR
  val lr = cur.mem === "b0001".U(4.W)
  val sc = cur.mem === "b0010".U(4.W)
  val reservedAddr = RegInit(0.U(32.W))
  when (valid && lr) { reservedAddr := cur.rdVal }
  when (valid && sc) { reservedAddr := 0.U }
  val scSucc = sc && reservedAddr === cur.rdVal
  val scFail = sc && reservedAddr =/= cur.rdVal
  out.bits.sc := scSucc
  out.bits.mem := Mux1H(Seq(
    lr -> "b1110".U(4.W),
    scFail -> 0.U(4.W),
    scSucc -> "b0110".U(4.W),
    !lrsc -> cur.mem
  ))
  out.bits.rdVal := Mux(scFail, 1.U(32.W), cur.rdVal)
  out.bits.fwReady := cur.fwReady || sc

  // 前递
  val gprFwIO = IO(new GprFwIO)
  gprFwIO.valid := valid
  gprFwIO.rd := cur.rd
  gprFwIO.ready := cur.fwReady || sc
  gprFwIO.fwVal := Mux(sc, scFail, cur.rdVal)

  // MMIO
  val inMem = cur.rdVal(31, 28) === "h8".U(4.W)
  val mmio = WireDefault(cur.mem.orR && !inMem) // 先验：LRSC不会访问MMIO
  val memReadIO = IO(new MemReadIO)
  val memWriteIO = IO(new MemWriteIO)

  // 状态机
  import MemPre.State._
  val state = RegInit(stIdle)
  val dataValid = RegInit(false.B)
  when (state === stIdle) {
    when (valid && mmio && !flush) {
      when (cur.mem(3)) { // load
        state := stRead
      } .otherwise { // store
        state := stWrite
        dataValid := true.B
      }
    }
  }
  when (memReadIO.resp) {
    state := stHold
    cur.rdVal := Mem.getLoadVal(cur.mem, cur.rdVal, memReadIO.data)
    cur.fwReady := true.B
    cur.mem := 0.U(4.W)
  }
  when (memWriteIO.dataReady) { dataValid := false.B }
  when (memWriteIO.resp) { state := stHold }
  when (out.fire) { state := stIdle }
  /*
  * 接到flush信号最早在req拉高的下一周期
  * 这时由于组合逻辑拉低了req，所以总线事务还未开始
  * 直接取消总线事务即可（详细解释见下）
  * */
  when (flush) { state := stIdle }
  memReadIO.req := state === stRead && !flush // flush通过组合逻辑拉低req
  memReadIO.addr := cur.rdVal
  memReadIO.size := cur.mem(1, 0)
  memReadIO.burst := 0.U(2.W) // fixed
  memReadIO.len := 0.U(8.W)
  memWriteIO.req := state === stWrite && !flush
  memWriteIO.addr := cur.rdVal
  memWriteIO.size := cur.mem(1, 0)
  memWriteIO.burst := 0.U(2.W) // fixed
  memWriteIO.len := 0.U(8.W)
  memWriteIO.dataValid := dataValid
  memWriteIO.data := cur.data << Cat(cur.rdVal(1, 0), 0.U(3.W))
  memWriteIO.strb := Cat(Fill(2, cur.mem(1)),
    cur.mem(1, 0).orR, 1.U(1.W)) << cur.rdVal(1, 0)
  memWriteIO.last := true.B
  setOutCond(!mmio || state === stHold)

  // TODO: Dcache冲刷时写回

  if (debug) {
    out.bits.inst.get := cur.inst.get
    out.bits.dnpc.get := cur.dnpc.get
    out.bits.skip.get := mmio
  }
}
