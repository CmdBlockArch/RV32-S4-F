package core.csr

import chisel3._

class CsrReadIO extends Bundle {
  val addr = Output(UInt(12.W))
  val data = Input(UInt(32.W))
  val err = Input(Bool())
}

class CsrWriteIO extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(12.W))
  val data = Output(UInt(32.W))
}

class Csr extends Module {
  val csrRegFile = new CsrRegFile

  val csrReadIO = IO(Flipped(new CsrReadIO))
  val csrReadPort = new CsrReadPort(csrRegFile)
  val (csrReadData: UInt, csrReadErr: Bool) = csrReadPort(csrReadIO.addr)
  csrReadIO.data := csrReadData
  csrReadIO.err := csrReadErr

  val csrWriteIO = IO(Flipped(new CsrWriteIO))
  val csrWritePort = new CsrWritePort(csrRegFile)
  when (csrWriteIO.en) {
    csrWritePort(csrWriteIO.addr, csrWriteIO.data)
  }
}
