package utils

import chisel3._

class SRam(addrW: Int, dataW: Int) extends Module {
  val n = 1 << addrW
  val readIO = IO(new Bundle {
    val addr = Input(UInt(addrW.W))
    val data = Output(UInt(dataW.W))
  })
  val writeIO = IO(new Bundle {
    val en = Input(Bool())
    val addr = Input(UInt(addrW.W))
    val data = Input(UInt(dataW.W))
  })

  val reg = Reg(Vec(n, UInt(dataW.W)))

  // 同时读写时，写入优先
  readIO.data := Mux(writeIO.en && readIO.addr === writeIO.addr, writeIO.data, reg(readIO.addr))

  when (writeIO.en) {
    reg(writeIO.addr) := writeIO.data
  }
}
