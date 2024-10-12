package utils

import chisel3._
import chisel3.util._

object MuxLookup1H {
  def apply[T <: Data](key: UInt)(mapping: Seq[(UInt, T)]): T = {
    Mux1H(mapping.map{ case (k, v) => (key === k, v) })
  }
}
