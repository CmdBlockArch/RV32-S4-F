package core.decode

import chisel3._
import chisel3.util._
import utils.PiplineModule
import core.fetch.FetchOut

class DecodeOut extends Bundle {
  val valA = Output(UInt(32.W))
  val valB = Output(UInt(32.W))
  val valC = Output(UInt(32.W))
  val func = Output(UInt(3.W))

  val rd = Output(UInt(5.W))

  val memEn = Output(Bool())
  val memOp = Output(UInt(4.W))

  val zicsr = Output(Bool())
  val csrAddr = Output(UInt(12.W))

  val ret = Output(UInt(2.W))
  val fenceI = Output(Bool())
  val fenceVMA = Output(Bool())

  val pc = Output(UInt(32.W))

  val trap = Output(Bool())
  val cause = Output(UInt(4.W))
}

class Decode extends PiplineModule(new FetchOut, new DecodeOut) {
  override def outCond = true.B

  out.bits := DontCare
}
