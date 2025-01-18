package core.decode

object OpcodeRawStr {
  val LUI    = "0110111"
  val AUIPC  = "0010111"
  val JAL    = "1101111"
  val JALR   = "1100111"
  val BRANCH = "1100011"
  val LOAD   = "0000011"
  val STORE  = "0100011"
  val CALRI  = "0010011"
  val CALRR  = "0110011"
  val SYSTEM = "1110011"
  val FENCE  = "0001111"
  val ATOMIC = "0101111"
}
