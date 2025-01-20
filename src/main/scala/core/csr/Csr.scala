package core.csr

import chisel3._

class CsrReadIO extends Bundle {
  val addr = Output(UInt(12.W))
  val data = Input(UInt(32.W))
  val err = Input(Bool())
}

class Csr extends Module {
  val csrRegFile = new CsrRegFile

  val csrReadIO = IO(Flipped(new CsrReadIO))
  val (csrReadData: UInt, csrReadErr: Bool) = csrRegFile.getCsrSrc(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr
}
