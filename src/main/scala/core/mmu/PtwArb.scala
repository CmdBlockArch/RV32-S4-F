package core.mmu

import chisel3._

class PtwArb extends Module {
  val master = IO(Vec(2, Flipped(MmuBundle.walkerIO)))
  val slave = IO(MmuBundle.walkerIO)

  val req = RegInit(false.B)
  val sel = Reg(UInt(1.W))

  when (!req) {
    when (master(1).req) {
      req := true.B
      sel := 1.U
    } .elsewhen (master(0).req) {
      req := true.B
      sel := 0.U
    }
  }
  when (slave.valid) {
    req := false.B
  }

  val cur = master(sel)
  slave.req := req
  slave.ptn := cur.ptn
  slave.vpn := cur.vpn
  master(0).value := slave.value
  master(1).value := slave.value
  master(0).valid := slave.valid && sel === 0.U
  master(1).valid := slave.valid && sel === 1.U
}
