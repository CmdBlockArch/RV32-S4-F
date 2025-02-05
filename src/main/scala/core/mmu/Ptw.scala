package core.mmu

import chisel3._
import chisel3.util._
import core.misc.MemReadIO

class Ptw extends Module {
  val io = IO(Flipped(MmuBundle.walkerIO))
  val memReadIO = IO(new MemReadIO)
  memReadIO.setBurst()

  val ptn = Reg(UInt(20.W))
  val vpn = Reg(UInt(10.W))
  val pteAddr = Cat(ptn, vpn, 0.U(2.W))

  val pte = WireDefault(memReadIO.data)
  val pteLeaf = pte(3, 1).orR
  val pteV = pte(0)
  val pteR = pte(1)
  val pteW = pte(2)
  val pteX = pte(3)
  val pteU = pte(4)
  val pteA = pte(6)
  val pteD = pte(7)

  val res = Reg(MmuBundle.resReg)

  import Walker.State._
  val state = RegInit(stIdle)
  val idle = state === stIdle
  val walk1 = state === stWalk1
  val walk0 = state === stWalk0
  val hold = state === stHold

  when (idle && io.req) {
    state := stWalk1
    ptn := io.ptn
    vpn := io.vpn1
  }
  when (memReadIO.resp) {
    when (pteV) {
      when (pteLeaf) {
        state := stHold
        res.mega := walk1
        res.read := pteR
        res.write := pteW
        res.exec := pteX
        res.user := pteU
        res.access := pteA
        res.dirty := pteD
        res.ppn := pte(29, 10)
      } .elsewhen (walk1) {
        state := stWalk0
        ptn := pte(29, 10)
        vpn := io.vpn0
      } .otherwise {
        state := stHold
        res.access := false.B // 无效页，必定触发PageFault
      }
    } .otherwise {
      state := stHold
      res.access := false.B
    }
  }
  when (hold) { state := stIdle }

  memReadIO.req := walk1 || walk0
  memReadIO.addr := pteAddr

  io.value := res
  io.valid := hold
}

object Walker {
  object State extends ChiselEnum {
    val stIdle, stWalk1, stWalk0, stHold = Value
  }
}
