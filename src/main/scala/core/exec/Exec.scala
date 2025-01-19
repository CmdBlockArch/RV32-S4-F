package core.exec

import chisel3._
import core.decode.DecodeOut
import utils.PiplineModule

class ExecOut extends Bundle {

}

class Exec extends PiplineModule(new DecodeOut, new ExecOut) {
  override def outCond = true.B

}
