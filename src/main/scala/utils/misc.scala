package utils

import chisel3._
import chisel3.util._

object MuxLookup1H {
  def apply[T <: Data](key: UInt)(mapping: Seq[(UInt, T)]): T = {
    Mux1H(mapping.map{ case (k, v) => (key === k, v) })
  }
}

object BitPat1H {
  def apply(width: Int, index: Int): BitPat = {
    val s = s"b${"0" * (width - index - 1)}1${"0" * index}"
    BitPat(s)
  }
}
