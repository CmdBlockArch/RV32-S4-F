package core.csr

import chisel3._
import chisel3.util._

class CsrWritePort(r: CsrRegFile) {
  def apply(addr: UInt, data: UInt) = {
    switch (addr) {
      is ("h100".U(12.W)) { // sstatus
        r.SIE := data(1)
        r.SPIE := data(5)
        r.SPP := data(8)
        r.SUM := data(18)
        r.MXR := data(19)
      }
      is ("h104".U(12.W)) { // sie
        r.SSIE := data(1)
        r.STIE := data(5)
        r.SEIE := data(9)
      }
      is ("h105".U(12.W)) { r.stvec := data }
      is ("h140".U(12.W)) { r.sscratch := data }
      is ("h141".U(12.W)) { r.sepc := data }
      is ("h142".U(12.W)) { r.scause := data }
      is ("h143".U(12.W)) { r.stval := data }
      is ("h180".U(12.W)) { r.satp := data }

      is ("h300".U(12.W)) { // mstatus
        r.SIE := data(1)
        r.MIE := data(3)
        r.SPIE := data(5)
        r.MPIE := data(7)
        r.SPP := data(8)
        r.MPP := data(12, 11)
        r.MPRV := data(17)
        r.SUM := data(18)
        r.MXR := data(19)
        r.TVM := data(20)
        r.TW := data(21)
        r.TSR := data(22)
      }
      is ("h302".U(12.W)) { r.medeleg := data }
      is ("h303".U(12.W)) { r.mideleg := data & "h222".U(32.W) }
      is ("h304".U(12.W)) { // mie
        r.SSIE := data(1)
        r.MSIE := data(3)
        r.STIE := data(5)
        r.MTIE := data(7)
        r.SEIE := data(9)
        r.MEIE := data(11)
      }
      is ("h305".U(12.W)) { r.mtvec := data }
      is ("h312".U(12.W)) { r.medelegh := data }
      is ("h340".U(12.W)) { r.mscratch := data }
      is ("h341".U(12.W)) { r.mepc := data }
      is ("h342".U(12.W)) { r.mcause := data }
      is ("h343".U(12.W)) { r.mtval := data }
      is ("h344".U(12.W)) { // mip
        r.SSIP := data(1)
        r.STIP := data(5)
        r.SEIP := data(9)
      }
    }
  }
}
