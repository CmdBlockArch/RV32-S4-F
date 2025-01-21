package core.exec

import chisel3._
import chisel3.util._

class Div extends Module {
  val in = IO(new Bundle {
    val ready = Output(Bool())
    val valid = Input(Bool())
    val sign = Input(Bool())
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
  })
  val out = IO(new Bundle {
    val ready = Input(Bool())
    val valid = Output(Bool())
    val quot = Output(UInt(32.W))
    val rem = Output(UInt(32.W))
  })
  val flush = IO(Input(Bool()))

  class DIV_test extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Reset())
      val flush = Input(Bool())

      val in_ready = Output(Bool())
      val in_valid = Input(Bool())
      val in_sign = Input(Bool())
      val in_a = Input(UInt(32.W))
      val in_b = Input(UInt(32.W))

      val out_ready = Input(Bool())
      val out_valid = Output(Bool())
      val out_quot = Output(UInt(32.W))
      val out_rem = Output(UInt(32.W))
    })
    addResource("/DIV_test.sv")
  }

  val div = Module(new DIV_test)
  div.io.clock := clock
  div.io.reset := reset
  div.io.flush := flush
  in.ready := div.io.in_ready
  div.io.in_valid := in.valid
  div.io.in_sign := in.sign
  div.io.in_a := in.a
  div.io.in_b := in.b
  div.io.out_ready := out.ready
  out.valid := div.io.out_valid
  out.quot := div.io.out_quot
  out.rem := div.io.out_rem
}
