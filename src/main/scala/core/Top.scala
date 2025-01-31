package core

import chisel3._
import chisel3.experimental.dataview._

import utils.Config._

import core.fetch.Fetch
import core.decode.Decode
import core.exec.Exec
import core.mem.{MemPre, Mem, DataCache}
import core.wb.{WriteBack, WbDebugOut}

import core.gpr.RegFile
import core.misc.{MemReadArb, MemWriteArb}

import perip.{SimMemRead, SimMemWrite, AxiReadIO, AxiWriteIO}

class Top extends Module {
  val gpr = Module(new RegFile)
  val dcache = Module(new DataCache.dcacheFactory.CacheWay)
  val memReadArb = Module(new MemReadArb(3))
  val memWriteArb = Module(new MemWriteArb(2))

  val fetch = Module(new Fetch)
  val decode = Module(new Decode); decode.in :<>= fetch.out
  val exec = Module(new Exec); exec.in :<>= decode.out
  val memPre = Module(new MemPre); memPre.in :<>= exec.out
  val mem = Module(new Mem); mem.in :<>= memPre.out
  val wb = Module(new WriteBack); wb.in :<>= mem.out

  // fetch
  fetch.io.flush := exec.io.jmp || wb.io.flush
  fetch.io.dnpc := Mux(wb.io.flush, wb.io.dnpc, exec.io.dnpc)
  fetch.io.fenceI := wb.io.fenceI
  memReadArb.master(0) :<>= fetch.memReadIO

  // decode
  decode.flush := exec.io.jmp || wb.io.flush
  gpr.readIO :<>= decode.gprReadIO
  wb.csrReadIO :<>= decode.csrReadIO
  decode.io.priv := wb.io.priv
  decode.io.mstatusTSR := wb.io.mstatusTSR
  decode.io.mstatusTVM := wb.io.mstatusTVM
  decode.io.mstatusTW := wb.io.mstatusTW

  // exec
  exec.flush := wb.io.flush
  gpr.fwIO(0) :<>= exec.gprFwIO

  // memPre
  memPre.flush := wb.io.flush
  dcache.readIO :<>= memPre.dcacheReadIO
  memReadArb.master(1) :<>= memPre.memReadIO
  memWriteArb.master(0) :<>= memPre.memWriteIO
  gpr.fwIO(1) :<>= memPre.gprFwIO

  // mem
  mem.flush := wb.io.flush
  dcache.writeIO :<>= mem.dcacheWriteIO
  memReadArb.master(2) :<>= mem.memReadIO
  memWriteArb.master(1) :<>= mem.memWriteIO
  gpr.fwIO(2) :<>= mem.gprFwIO

  // wb
  gpr.writeIO :<>= wb.gprWriteIO
  dcache.io.flush := wb.io.fenceI || wb.io.fenceVMA

  if (synthesis) { // axi port
    val axiReadIO = IO(new AxiReadIO)
    val axiWriteIO = IO(new AxiWriteIO)
    axiReadIO :<>= memReadArb.slave
    axiWriteIO :<>= memWriteArb.slave
  } else { // simMem
    val simMemRead = Module(new SimMemRead)
    val simMemWrite = Module(new SimMemWrite)
    simMemRead.io :<>= memReadArb.slave
    simMemWrite.io :<>= memWriteArb.slave
  }

  // debug
  val debugIO = if (debug) {
    Some(IO(new WbDebugOut {
      val gpr = Output(Vec(32, UInt(32.W)))
    }))
  } else None
  if (debug) {
    debugIO.get.viewAsSupertype(new WbDebugOut) := wb.debugOut.get
    debugIO.get.gpr := gpr.debugOut.get
  }
}
