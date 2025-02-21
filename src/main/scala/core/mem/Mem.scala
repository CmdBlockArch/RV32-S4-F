package core.mem

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}
import utils.PiplineModule
import utils.Config._
import core.gpr.GprFwIO
import core.misc.{MemReadIO, MemWriteIO}
import DataCache.{dcacheFactory => dc}
import core.csr.Priv

class MemOut extends Bundle {
  /*
  * 除了GPR之外的控制信号生效时，都不需要进行访存
  * 所以只有GPR需要等待流水级的valid信号
  * 其他信号在Mem阶段就已经有效，故将valid直通给WB阶段
  * 使得这些信号的valid时序路径更短
  * */
  val valid = Output(Bool())
  val vaddr = Output(UInt(32.W))
  // GPR
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
  val dnpc = Output(UInt(32.W))
  val trap = Output(Bool())
  val intr = Output(UInt(2.W))
  val cause = Output(UInt(4.W))
  val flush = Output(Bool())
  // 调试
  val inst = DebugOutput(UInt(32.W))
  val skip = DebugOutput(Bool())

  def retEn = ret.orR
  def mret = ret === Priv.M
  def sret = ret === Priv.S
}

class Mem extends PiplineModule(new MemPreOut, new MemOut) {
  val memReadIO = IO(new MemReadIO)
  val memWriteIO = IO(new MemWriteIO)
  val sb = Module(new StoreBuffer)
  memWriteIO :<>= sb.memWriteIO
  val io = IO(new Bundle {
    val sbEmpty = Output(Bool())
  })
  io.sbEmpty := sb.io.empty

  // 内存操作控制信号
  val mem = cur.mem.orR
  val load = cur.mem(3)
  val store = cur.mem(3, 2) === "b01".U(2.W)
  val amo = cur.mem === "b0011".U(4.W)

  // 地址翻译结果
  val vaddr = cur.rdVal
  val paddr = Cat(cur.ppn, cur.rdVal(11, 0))

  // cache写入生成
  val gen = RegInit(dc.WriteIO.initGen)
  val dcacheWriteIO = IO(new dc.WriteIO)
  dcacheWriteIO := gen

  // 当前指令读取/写入的地址，vipt
  val tag = dc.getTag(paddr)
  val index = dc.getIndex(paddr)
  val offset = dc.getOffset(paddr)

  // 命中判断
  val genHit = gen.valid && gen.index === index && gen.tag === tag
  val curHitVec = dc.tagHitVec(cur.dcacheValid, cur.dcacheTag, tag)
  val dcHit = genHit || curHitVec.reduce(_ || _)
  val dcWay = Mux(genHit, gen.way, curHitVec.onlyIndexWhere(_.asBool))
  val dcData = Mux(genHit, gen.data.asUInt,
    Mux1H(curHitVec, cur.dcacheData).asUInt).asTypeOf(dc.wayDataType)

  // 二路组相联 dcache PLRU
  val plru = Reg(dc.plruType)
  val dcEvictWay = ~plru(index)

  // 状态机
  val req = RegInit(false.B)
  val burstOffset = Reg(UInt((dc.offsetW - 2).W))
  sb.io.lock := valid && (load || amo) && !dcHit
  when (!req && sb.io.locked) {
    req := true.B
    gen.valid := false.B
    gen.index := index
    gen.way := dcEvictWay
    gen.tag := tag
    cur.dcacheValid := 0.U.asTypeOf(dc.validType)
    burstOffset := 0.U
  }
  when (memReadIO.resp) {
    when (sb.readIO.valid) {
      gen.data(burstOffset) := sb.readIO.merge(memReadIO.data)
    } .otherwise {
      gen.data(burstOffset) := memReadIO.data
    }
    burstOffset := burstOffset + 1.U
    when (memReadIO.last) {
      req := false.B
      gen.valid := true.B
    }
  }
  memReadIO.req := req
  memReadIO.addr := Cat(paddr(31, dc.offsetW), 0.U(dc.offsetW.W))
  memReadIO.setBurst(dc.blockN)
  sb.readIO.addr := Cat(paddr(31, dc.offsetW), burstOffset, 0.U(2.W))

  // load/store结果
  val data = dcData(offset)
  val loadVal = Wire(UInt(32.W)); loadVal := Mem.getLoadVal(cur.mem, paddr, data)
  val amoVal = Wire(UInt(32.W)); amoVal := Mem.getAmoVal(cur.amoFunc, data, cur.data)

  val stData = Wire(UInt(32.W)); stData := cur.data
  val wstrb = Wire(UInt(4.W)); wstrb := Cat(Fill(2, cur.mem(1)), cur.mem(1, 0).orR, 1.U(1.W)) << paddr(1, 0)
  val wmask = Cat(Fill(8, wstrb(3)), Fill(8, wstrb(2)), Fill(8, wstrb(1)), Fill(8, wstrb(0)))
  val storeVal = Wire(UInt(32.W)); storeVal := (data & ~wmask) | (stData & wmask)

  // dcache写入
  when (valid && mem && dcHit) {
    gen.valid := true.B
    gen.index := index
    gen.way := dcWay
    gen.tag := tag
    gen.data := dcData
    when (store) { gen.data(offset) := storeVal }
    when (amo) { gen.data(offset) := amoVal }
    when (load) { plru(index) := dcWay }
  }

  // store buffer写入
  val storeValid = valid && (store || (amo && dcHit))
  sb.writeIO.req := storeValid
  sb.writeIO.addr := paddr
  sb.writeIO.data := Mux(store, stData, amoVal)
  sb.writeIO.strb := wstrb
  val sbResp = sb.writeIO.resp

  // 阶段完成条件
  setOutCond(!mem || (store && sbResp) || (dcHit && (load || sbResp)))

  // 前递
  val gprFwIO = IO(new GprFwIO)
  gprFwIO.valid := valid
  gprFwIO.rd := cur.rd
  gprFwIO.ready := cur.fwReady || ((load || amo) && dcHit)
  val rdVal = Mux(load || amo, loadVal, Mux(cur.sc, 0.U, cur.rdVal))
  gprFwIO.fwVal := rdVal

  // 输出
  out.bits.valid := valid
  out.bits.vaddr := cur.rdVal
  out.bits.rd := cur.rd
  out.bits.rdVal := rdVal
  out.bits.csrWen := cur.csrWen
  out.bits.csrAddr := cur.csrAddr
  out.bits.csrData := cur.data
  out.bits.ret := cur.ret
  out.bits.fenceI := cur.fenceI
  out.bits.fenceVMA := cur.fenceVMA
  out.bits.pc := cur.pc
  out.bits.dnpc := cur.dnpc
  out.bits.trap := cur.trap
  out.bits.intr := cur.intr
  out.bits.cause := cur.cause
  out.bits.flush := cur.flush

  if (debug) {
    out.bits.inst.get := cur.inst.get
    out.bits.skip.get := cur.skip.get
  }
}

object Mem {
  val amoTruthTable = TruthTable(
    Map(
      BitPat("b0001") -> BitPat("b000000001"),
      BitPat("b0000") -> BitPat("b000000010"),
      BitPat("b0010") -> BitPat("b000000100"),
      BitPat("b0110") -> BitPat("b000001000"),
      BitPat("b0100") -> BitPat("b000010000"),
      BitPat("b1000") -> BitPat("b000100000"),
      BitPat("b1010") -> BitPat("b001000000"),
      BitPat("b1100") -> BitPat("b010000000"),
      BitPat("b1110") -> BitPat("b100000000"),
    ), BitPat.dontCare(9)
  )
  def getLoadVal(mem: UInt, addr: UInt, old: UInt): UInt = {
    val offset = Cat(addr(1, 0), 0.U(3.W))
    val rdata = Wire(UInt(32.W)); rdata := old >> offset
    Mux(mem(1), rdata, Mux(mem(0), // amo操作码刚好对应lw，且地址一定对齐
      Cat(Fill(16, rdata(15) && mem(2)), rdata(15, 0)), // 01
      Cat(Fill(24, rdata( 7) && mem(2)), rdata( 7, 0)), // 00
    ))
  }
  def getAmoVal(func: UInt, old: UInt, data: UInt): UInt = {
    val func1H = decoder(func, amoTruthTable)
    val add = func1H(1)
    val a = Wire(UInt(32.W)); a := old
    val b = Wire(UInt(32.W)); b := data ^ Fill(32, !add)
    val e = (a +& b) + (!add).asUInt
    val cf = e(32)
    val sf = e(31)
    val of = (a(31) === b(31)) && (sf ^ a(31))
    val lts = sf ^ of
    val geu = cf
    Mux1H(func1H, Seq(
      data, // swap
      e(31, 0), // add
      old ^ data, // xor
      old & data, // and
      old | data, // or
      Mux(lts, old, data), // min
      Mux(lts, data, old), // max
      Mux(geu, data, old), // minu
      Mux(geu, old, data), // maxu
    ))
  }
}
