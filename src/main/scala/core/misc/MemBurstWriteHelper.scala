package core.misc

import chisel3._

class MemBurstWriteHelper(val len: Int) {
  class MasterIO extends Bundle {
    val req = Output(Bool())
    val addr = Output(UInt(32.W))
    val data = Output(Vec(len, UInt(32.W)))

    val resp = Input(Bool())
    val err = Input(Bool())
  }

  class BurstWriteModule extends Module {
    val master = IO(Flipped(new MasterIO))
    val slave = IO(new MemWriteIO)
    slave.req := master.req
    slave.addr := master.addr
    slave.size := "b10".U
    slave.burst := "b10".U
    slave.len := (len - 1).U

    val running = RegInit(false.B)
    val dataValid = RegInit(false.B)
    val offset = Reg(UInt(8.W))
    slave.dataValid := dataValid
    slave.data := master.data(offset)
    slave.strb := "b1111".U
    slave.last := offset === (len - 1).U

    when (running) {
      when (slave.dataReady) {
        offset := offset + 1.U
        when (slave.last) {
          dataValid := false.B
        }
      }
    } .otherwise {
      when (master.req) {
        running := true.B
        dataValid := true.B
        offset := 0.U
      }
    }

    when (slave.resp) {
      running := false.B
    }

    master.resp := slave.resp
    master.err := slave.err
  }

  def apply() = new BurstWriteModule
}