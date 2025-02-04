package core.mmu

import chisel3._

class MmuIO extends Bundle {
  val req = Output(Bool())
  val mode = Output(UInt(2.W)) // 00: fetch, 01: load, 10: store
  val vpn = Output(UInt(20.W))

  val resp = Input(Bool())
  val pf = Input(Bool())
  val ppn = Input(UInt(20.W))
}

class Mmu extends Module {

}
