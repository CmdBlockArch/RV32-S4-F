package core.mem

import chisel3._
import chisel3.util._
import core.misc.{MemWriteIO, MemBurstWriteHelper}

class StoreBuffer(size: Int = 16) extends Module {
  val io = IO(new Bundle {
    val lock = Input(Bool())
    val locked = Output(Bool())

    val full = Output(Bool())
    val empty = Output(Bool())
  })

  val r = RegInit(VecInit(Seq.fill(size){
    val t = Wire(new Bundle {
      val valid = Bool()
      val addr = UInt(30.W)
      val data = UInt(32.W)
    })
    t := DontCare
    t.valid := false.B
    t
  }))

  val valid = VecInit(r.map(_.valid))
  val full = valid.reduce(_ && _)
  io.full := full
  val empty = !valid.reduce(_ || _)
  io.empty := empty
  val freeIdx = valid.indexWhere(!_.asBool)
  val validIdx = valid.indexWhere(_.asBool)

  val readIO = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val valid = Output(Bool())
    val data = Output(UInt(32.W))
  })
  val readValid = VecInit(r.map(t => t.valid && t.addr === readIO.addr(31, 2)))
  readIO.valid := readValid.reduce(_ || _)
  readIO.data := Mux1H(readValid, r.map(_.data))

  val writeIO = IO(new Bundle {
    val req = Input(Bool())
    val addr = Input(UInt(32.W))
    val data = Input(UInt(32.W))
    val resp = Output(Bool())

    def fire = req && resp
  })
  val wCover = VecInit(r.map(t => t.valid && t.addr === writeIO.addr(31, 2)))
  val wCoverEn = wCover.reduce(_ || _)
  val wCoverIdx = Mux1H(wCover, (0 until size).map(_.U))
  writeIO.resp := !full || wCoverEn
  val wIdx = Mux(wCoverEn, wCoverIdx, freeIdx)
  when (writeIO.fire) {
    r(wIdx).valid := true.B
    r(wIdx).addr := writeIO.addr(31, 2)
    r(wIdx).data := writeIO.data
  }

  val memWriteIO = IO(new MemWriteIO)
  val memBw = Module((new MemBurstWriteHelper())())
  memWriteIO :<>= memBw.slave
  val memBwIO = memBw.master

  import StoreBuffer.State._
  val state = RegInit(stIdle)
  val idle = state === stIdle
  val actIdx = Reg(UInt(log2Up(size).W))
  val actAddr = r(actIdx).addr
  val actData = Reg(UInt(32.W))
  when (idle && !empty && !io.lock) {
    state := stStore
    actIdx := validIdx
    actData := r(validIdx).data
  }
  when (memBwIO.fire) {
    state := stIdle
    when (r(actIdx).data === actData && !(writeIO.fire && wIdx === actIdx)) {
      r(actIdx).valid := false.B
    }
  }
  memBwIO.req := state === stStore
  memBwIO.addr := Cat(actAddr, 0.U(2.W))
  memBwIO.data(0) := actData
  io.locked := io.lock && idle
}

object StoreBuffer {
  object State extends ChiselEnum {
    val stIdle, stStore = Value
  }
}
