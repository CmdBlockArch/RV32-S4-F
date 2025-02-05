package core.mmu

import chisel3._
import chisel3.experimental.dataview._

class MmuValue extends Bundle {
  val valid = Input(Bool())

  // 4MB大页
  val mega = Input(Bool())
  // 权限位
  val read = Input(Bool())
  val write = Input(Bool())
  val exec = Input(Bool())
  val user = Input(Bool())
  // A/D位
  val access = Input(Bool())
  val dirty = Input(Bool())
  // 物理页号
  val ppn = Input(UInt(20.W))

  def value = this.viewAsSupertype(new MmuValue)
}

trait MmuVpn {
  // 虚拟页号
  val vpn = Output(UInt(20.W))
  def vpn1 = vpn(19, 10)
  def vpn0 = vpn(9, 0)
}

class MmuBundle extends MmuValue with MmuVpn

object MmuBundle {
  def resReg = Output(new MmuValue)
  def tlbReg = Output(new MmuBundle)
  def tlbRead = new MmuBundle
  def tlbWrite = Output(new MmuBundle)
  def walkerIO = new MmuBundle {
    val req = Output(Bool())
    val ptn = Output(UInt(20.W))
  }
}
