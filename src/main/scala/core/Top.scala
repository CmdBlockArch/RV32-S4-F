package core

import chisel3._
import chisel3.experimental.dataview._
import utils.Config._
import core.fetch.{InstCache, FetchPc, Fetch}
import core.decode.Decode
import core.exec.Exec
import core.mem.{DataCache, Mem, MemPre}
import core.wb.{WbDebugOut, WriteBack}
import core.gpr.RegFile
import core.fetch.Bpu
import core.misc.{MemReadArb, MemWriteArb}
import core.mmu.{Mmu, Ptw, PtwArb}
import perip.{AxiReadIO, AxiWriteIO, SimMemRead, SimMemWrite}

class Top extends Module {
  val gpr = Module(new RegFile)
  val iCache = Module(new InstCache.icacheFactory.Cache)
  val dCache = Module(new DataCache.dcacheFactory.Cache)
  val bpu = Module(new Bpu.factory.BrPred)
  val memReadArb = Module(new MemReadArb(4))
  val memWriteArb = Module(new MemWriteArb(2))

  val fetchPc = Module(new FetchPc)
  val fetch = Module(new Fetch); fetch.in :<>= fetchPc.out
  val decode = Module(new Decode); decode.in :<>= fetch.out
  val exec = Module(new Exec); exec.in :<>= decode.out
  val memPre = Module(new MemPre); memPre.in :<>= exec.out
  val mem = Module(new Mem); mem.in :<>= memPre.out
  val wb = Module(new WriteBack); wb.in :<>= mem.out

  val ptw = Module(new Ptw)
  memReadArb.master(3) :<>= ptw.memReadIO
  val ptwArb = Module(new PtwArb)
  ptw.io :<>= ptwArb.slave
  def mkMmu(i: Int) = {
    val mmu = Module(new Mmu)
    mmu.io.fenceVMA := wb.io.fenceVMA
    mmu.io.priv := wb.io.priv
    mmu.io.satp := wb.io.satp
    mmu.io.mstatusMPP := wb.io.mstatusMPP
    mmu.io.mstatusMPRV := wb.io.mstatusMPRV
    mmu.io.mstatusSUM := wb.io.mstatusSUM
    mmu.io.mstatusMXR := wb.io.mstatusMXR
    ptwArb.master(i) :<>= mmu.ptwIO
    mmu
  }

  // 分支结果产生前的flush信号
  val preBrFlush = exec.io.jmp || wb.io.flush

  // fetchPc
  fetchPc.io.flush := preBrFlush
  fetchPc.io.dnpc := Mux(wb.io.flush, wb.io.dnpc, exec.io.dnpc)
  bpu.predIO :<>= fetchPc.bpuPredIO
  iCache.readIO :<>= fetchPc.icReadIO

  // fetch
  fetch.io.flush := preBrFlush
  memReadArb.master(0) :<>= fetch.memReadIO
  val iMmu = mkMmu(0)
  iMmu.io.flush := wb.io.flush
  iMmu.mmuIO :<>= fetch.mmuIO
  iCache.writeIO :<>= fetch.icacheWriteIO

  // decode
  decode.flush := preBrFlush
  gpr.readIO :<>= decode.gprReadIO
  wb.csrReadIO :<>= decode.csrReadIO
  decode.io.priv := wb.io.priv
  decode.io.mstatusTVM := wb.io.mstatusTVM
  decode.io.mstatusTW := wb.io.mstatusTW
  decode.io.mstatusTSR := wb.io.mstatusTSR

  // exec
  exec.flush := wb.io.flush
  bpu.trainIO :<>= exec.bpuTrainIO
  gpr.fwIO(0) :<>= exec.gprFwIO

  // memPre
  memPre.flush := wb.io.flush
  memPre.io.sbEmpty := mem.io.sbEmpty
  dCache.readIO :<>= memPre.dcacheReadIO
  memReadArb.master(1) :<>= memPre.memReadIO
  memWriteArb.master(1) :<>= memPre.memWriteIO
  gpr.fwIO(1) :<>= memPre.gprFwIO
  val dMmu = mkMmu(1)
  dMmu.io.flush := false.B
  dMmu.mmuIO :<>= memPre.mmuIO

  // mem
  mem.flush := false.B
  dCache.writeIO :<>= mem.dcacheWriteIO
  memReadArb.master(2) :<>= mem.memReadIO
  memWriteArb.master(0) :<>= mem.memWriteIO
  gpr.fwIO(2) :<>= mem.gprFwIO

  // wb
  gpr.writeIO :<>= wb.gprWriteIO
  iCache.io.flush := wb.io.fenceI
  dCache.io.flush := wb.io.fenceI
  bpu.io.flush := wb.io.fenceI

  if (debug) { // simMem
    val simMemRead = Module(new SimMemRead)
    val simMemWrite = Module(new SimMemWrite)
    simMemRead.io :<>= memReadArb.slave
    simMemWrite.io :<>= memWriteArb.slave
  } else { // axi port
    val axiReadIO = IO(new AxiReadIO)
    val axiWriteIO = IO(new AxiWriteIO)
    axiReadIO :<>= memReadArb.slave
    axiWriteIO :<>= memWriteArb.slave
  }

  // debug
  val debugIO = DebugIO(new WbDebugOut {
    val gpr = Output(Vec(32, UInt(32.W)))
  })
  if (debug) {
    debugIO.get.viewAsSupertype(new WbDebugOut) := wb.debugOut.get
    debugIO.get.gpr := gpr.debugOut.get
  }
}
