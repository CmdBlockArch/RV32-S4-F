import core.misc.{MemReadArb, MemWriteArb}

object Elaborate extends App {
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).mkString(","))
  circt.stage.ChiselStage.emitSystemVerilogFile(new MemReadArb(3), args, firtoolOptions)
}
