package core.fetch

import chisel3._

class FetchIO extends Bundle {
  val ready = Input(Bool())
  val valid = Output(Bool())

  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
}

class Fetch extends Module {

}
