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
import core.mmu.MmuIO
import utils.Config._

class MemPreOut extends ExecOut {
  // mmu
  val ppn = Output(UInt(20.W))
  // sc
  val sc = Output(Bool())
  // Data Cache
  val dcacheValid = Output(Bool())
  val dcacheTag = Output(UInt(dc.tagW.W))
  val dcacheData = Output(dc.dataType)
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
  out.bits.dcacheTag := dcacheReadIO.tag
  out.bits.dcacheData := dcacheReadIO.data

  // 控制信号
  val mem = cur.mem.orR
  val lrsc = !cur.mem(3, 2).orR && cur.mem(1, 0).xorR
  val lr = cur.mem === "b0001".U(4.W)
  val sc = cur.mem === "b0010".U(4.W)

  // MMU
  val mmuIO = IO(new MmuIO)
  mmuIO.valid := valid && mem && !flush
  mmuIO.fetch := false.B
  mmuIO.load := cur.mem(3) || lr
  mmuIO.store := !cur.mem(3) && !lr
  mmuIO.vpn := cur.rdVal(31, 12)
  val ppn = mmuIO.ppn
  out.bits.ppn := ppn
  val mmuHit = mmuIO.hit
  val pf = mmuHit && mmuIO.pf
  val paddrValid = mmuHit && !mmuIO.pf

  // MMIO
  val inMem = ppn(19, 16) === "h8".U(4.W)
  val mmio = mem && !inMem // 假设：LRSC不会访问MMIO

  // LR&SC
  val reservedAddr = RegInit(0.U(32.W))
  when (valid && lr && paddrValid) { reservedAddr := cur.rdVal }
  when (valid && sc && paddrValid) { reservedAddr := 0.U }
  val scSucc = sc && reservedAddr === cur.rdVal
  val scFail = sc && reservedAddr =/= cur.rdVal
  out.bits.sc := scSucc
  // 当不需要访存时，无论mmuIO.pf值如何（事实上此时其值无意义），mem值都为0
  out.bits.mem := Mux(mmuIO.pf, 0.U(4.W), Mux1H(Seq(
    lr -> "b1110".U(4.W),
    scFail -> 0.U(4.W),
    scSucc -> "b0110".U(4.W),
    !lrsc -> Mux(mmio, 0.U(4.W), cur.mem)
  )))
  out.bits.rdVal := Mux(scFail, 1.U(32.W), cur.rdVal)
  out.bits.fwReady := cur.fwReady || sc

  // 前递
  val gprFwIO = IO(new GprFwIO)
  gprFwIO.valid := valid
  gprFwIO.rd := cur.rd
  gprFwIO.ready := cur.fwReady || sc
  gprFwIO.fwVal := Mux(sc, scFail, cur.rdVal)

  // MMIO访存状态机
  val memReadIO = IO(new MemReadIO)
  val memWriteIO = IO(new MemWriteIO)
  import MemPre.State._
  val state = RegInit(stIdle)
  val idle = state === stIdle
  val hold = state === stHold
  val dataValid = RegInit(false.B)
  val mmioPpn = Reg(UInt(20.W))
  when (idle) {
    when (valid && paddrValid && mmio && !flush) {
      mmioPpn := ppn
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
  }
  when (memWriteIO.dataReady) { dataValid := false.B }
  when (memWriteIO.resp) { state := stHold }
  when (out.fire) { state := stIdle }
  when (flush) { state := stIdle }
  val mmioAddr = Cat(mmioPpn, cur.rdVal(11, 0))
  memReadIO.req := state === stRead
  memReadIO.addr := mmioAddr
  memReadIO.size := cur.mem(1, 0)
  memReadIO.burst := 0.U(2.W) // fixed
  memReadIO.len := 0.U(8.W)
  memWriteIO.req := state === stWrite && !flush
  memWriteIO.addr := mmioAddr
  memWriteIO.size := cur.mem(1, 0)
  memWriteIO.burst := 0.U(2.W) // fixed
  memWriteIO.len := 0.U(8.W)
  memWriteIO.dataValid := dataValid
  memWriteIO.data := cur.data << Cat(cur.rdVal(1, 0), 0.U(3.W))
  memWriteIO.strb := Cat(Fill(2, cur.mem(1)),
    cur.mem(1, 0).orR, 1.U(1.W)) << cur.rdVal(1, 0)
  memWriteIO.last := true.B

  // fenceI sfenceVMA
  val io = IO(new Bundle {
    val sbEmpty = Input(Bool())
  })
  val fenceFinish = !cur.fenceI || io.sbEmpty // 等待StoreBuffer全部写回

  val memFinish = !mem || pf || (mmuHit && inMem) || hold
  setOutCond(memFinish && fenceFinish)
  val trap = cur.trap || (mem && pf)
  out.bits.trap := trap
  out.bits.rd := Mux(trap, 0.U, cur.rd)
  out.bits.cause := Mux(cur.trap, cur.cause, mmuIO.cause)
  out.bits.flush := cur.flush || (mem && pf)

  if (debug) {
    out.bits.skip.get := cur.skip.get || mmio
  }
}
