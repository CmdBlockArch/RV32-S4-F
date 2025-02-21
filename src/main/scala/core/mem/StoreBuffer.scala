package core.mem

import chisel3._
import chisel3.util._
import core.misc.{MemWriteIO, MemWriteHelper}

class StoreBuffer(size: Int = 32) extends Module {
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
      val strb = UInt(4.W)
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

  val readIO = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val valid = Output(Bool())
    val data = Output(UInt(32.W))
    val strb = Output(UInt(4.W))

    def merge(old: UInt) = {
      StoreBuffer.mergeReq(old, strb, data)
    }
  })
  val readValid = VecInit(r.map(t => t.valid && t.addr === readIO.addr(31, 2)))
  readIO.valid := readValid.reduce(_ || _)
  readIO.data := Mux1H(readValid, r.map(_.data))
  readIO.strb := Mux1H(readValid, r.map(_.strb))

  val writeIO = IO(new Bundle {
    val req = Input(Bool())
    val addr = Input(UInt(32.W))
    val data = Input(UInt(32.W))
    val strb = Input(UInt(4.W))
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
    when (wCoverEn) {
      r(wIdx).data := StoreBuffer.mergeReq(r(wIdx).data, writeIO.strb, writeIO.data)
      r(wIdx).strb := r(wIdx).strb | writeIO.strb
    } .otherwise {
      r(wIdx).data := writeIO.data
      r(wIdx).strb := writeIO.strb
    }
  }

  val memWriteIO = IO(new MemWriteIO)
  val memBw = Module((new MemWriteHelper())())
  memWriteIO :<>= memBw.slave
  val memBwIO = memBw.master

  import StoreBuffer.State._
  val state = RegInit(stIdle)
  val idle = state === stIdle

  val actIdx = valid.indexWhere(_.asBool)
  val actAddr = Reg(UInt(30.W))
  val actStrb = Reg(UInt(4.W))
  val actData = Reg(UInt(32.W))
  when (idle && !empty && !io.lock) {
    state := stStore
    actAddr := r(actIdx).addr
    actStrb := r(actIdx).strb
    actData := r(actIdx).data
    when (!(writeIO.fire && wIdx === actIdx)) {
      r(actIdx).valid := false.B
    }
  }
  when (memBwIO.fire) {
    state := stIdle
  }
  memBwIO.req := state === stStore
  memBwIO.addr := Cat(actAddr, 0.U(2.W))
  memBwIO.data := actData
  memBwIO.strb := actStrb
  io.locked := io.lock && idle
}

object StoreBuffer {
  object State extends ChiselEnum {
    val stIdle, stStore = Value
  }

  def mergeReq(old: UInt, strb: UInt, data: UInt) = {
    val vec = Wire(Vec(4, UInt(8.W)))
    vec := old.asTypeOf(vec)
    when (strb(0) === 1.U) { vec(0) := data(7, 0) }
    when (strb(1) === 1.U) { vec(1) := data(15, 8) }
    when (strb(2) === 1.U) { vec(2) := data(23, 16) }
    when (strb(3) === 1.U) { vec(3) := data(31, 24) }
    vec.asUInt
  }
}
