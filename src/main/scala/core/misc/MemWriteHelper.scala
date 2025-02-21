package core.misc

import chisel3._

class MemWriteHelper {
  class MasterIO extends Bundle {
    val req = Output(Bool())
    val addr = Output(UInt(32.W))
    val data = Output(UInt(32.W))
    val strb = Output(UInt(4.W))

    val resp = Input(Bool())
    val err = Input(Bool())

    def fire = req && resp
  }

  class WriteHelperModule extends Module {
    val master = IO(Flipped(new MasterIO))
    val slave = IO(new MemWriteIO)
    slave.req := master.req
    slave.addr := master.addr
    slave.size := "b10".U
    slave.burst := "b00".U // fixed
    slave.len := 0.U

    val running = RegInit(false.B)
    val dataValid = RegInit(false.B)
    slave.dataValid := dataValid
    slave.data := master.data
    slave.strb := master.strb
    slave.last := true.B

    when(running) {
      when(slave.dataReady) {
        dataValid := false.B
      }
    }.otherwise {
      when(master.req) {
        running := true.B
        dataValid := true.B
      }
    }

    when(slave.resp) {
      running := false.B
    }

    master.resp := slave.resp
    master.err := slave.err
  }

  def apply() = new WriteHelperModule
}
