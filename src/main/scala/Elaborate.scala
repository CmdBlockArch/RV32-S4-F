import core.misc.MemWriteArb

object Elaborate extends App {
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).mkString(","))
  circt.stage.ChiselStage.emitSystemVerilogFile(new MemWriteArb(3), args, firtoolOptions)
}
