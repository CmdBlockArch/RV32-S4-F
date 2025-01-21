package core.exec

import chisel3._
import chisel3.util._

class Mul extends Module {
  val in = IO(new Bundle {
    val ready = Output(Bool())
    val valid = Input(Bool())
    val sign = Input(UInt(2.W))
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
  })
  val out = IO(new Bundle {
    val ready = Input(Bool())
    val valid = Output(Bool())
    val prod = Output(UInt(64.W))
  })
  val flush = IO(Input(Bool()))

  class MUL_test extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Reset())
      val flush = Input(Bool())

      val in_ready = Output(Bool())
      val in_valid = Input(Bool())
      val in_sign = Input(UInt(2.W))
      val in_a = Input(UInt(32.W))
      val in_b = Input(UInt(32.W))

      val out_ready = Input(Bool())
      val out_valid = Output(Bool())
      val out_prod = Output(UInt(64.W))
    })
    addResource("/MUL_test.sv")
  }

  val mul = Module(new MUL_test)
  mul.io.clock := clock
  mul.io.reset := reset
  mul.io.flush := flush
  in.ready := mul.io.in_ready
  mul.io.in_valid := in.valid
  mul.io.in_sign := in.sign
  mul.io.in_a := in.a
  mul.io.in_b := in.b
  mul.io.out_ready := out.ready
  out.valid := mul.io.out_valid
  out.prod := mul.io.out_prod
}
