package core.mem

import chisel3._
import chisel3.experimental.dataview._
import utils.PiplineModule
import core.exec.ExecOut
import core.gpr.GprFwIO
import utils.Config._

import DataCache.{dcacheFactory => dc}

class MemPreOut extends ExecOut {
  val dcacheValid = Output(Bool())
  val dcacheDirty = Output(Bool())
  val dcacheTag = Output(UInt(dc.tagW.W))
  val dcacheData = Output(dc.dataType)
}

class MemPre extends PiplineModule(new ExecOut, new MemPreOut) {
  // 前递
  val gprFwIO = IO(new GprFwIO)
  gprFwIO.valid := valid
  gprFwIO.rd := cur.rd
  gprFwIO.ready := cur.fwReady
  gprFwIO.fwVal := cur.rdVal

  // dcache读
  val dcacheReadIO = IO(new dc.readIO)
  dcacheReadIO.index := dc.getIndex(cur.rdVal)
  out.bits.dcacheValid := dcacheReadIO.valid
  out.bits.dcacheDirty := dcacheReadIO.dirty
  out.bits.dcacheTag := dcacheReadIO.tag
  out.bits.dcacheData := dcacheReadIO.data

  out.bits.viewAsSupertype(new ExecOut) := cur

  // TODO: Dcache冲刷时写回

  if (debug) {
    out.bits.inst.get := cur.inst.get
    out.bits.dnpc.get := cur.dnpc.get
  }
}
