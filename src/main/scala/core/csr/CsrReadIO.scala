package core.csr

import chisel3._

class CsrReadIO extends Bundle {
  val addr = Output(UInt(12.W))
  val data = Input(UInt(32.W))
  val err = Input(Bool())
}
