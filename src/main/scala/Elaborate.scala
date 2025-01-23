import core.decode.Decode
import core.misc.{MemReadArb, MemWriteArb}
import utils.SRam
import core.fetch.Fetch
import core.decode.Decode
import core.exec.{Alu, Exec}
import core.csr.Csr
import core.mem.MemPre

object Elaborate extends App {
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).mkString(","))
  circt.stage.ChiselStage.emitSystemVerilogFile(new MemPre, args, firtoolOptions)
}
