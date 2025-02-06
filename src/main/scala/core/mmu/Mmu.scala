package core.mmu

import chisel3._
import chisel3.util._

class MmuIO extends Bundle {
  val valid = Output(Bool())
  val fetch = Output(Bool())
  val load = Output(Bool())
  val store = Output(Bool())
  val vpn = Output(UInt(20.W))

  val hit = Input(Bool())
  val pf = Input(Bool())
  val cause = Input(UInt(4.W))
  val ppn = Input(UInt(20.W))
}

class Mmu extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val fenceVMA = Input(Bool())

    val priv = Input(UInt(2.W))
    val satp = Input(UInt(32.W))
    val mstatusMPP = Input(UInt(2.W))
    val mstatusMPRV = Input(Bool())
    val mstatusSUM = Input(Bool())
    val mstatusMXR = Input(Bool())
  })
  val mmuIO = IO(Flipped(new MmuIO))

  val priv = Mux(io.mstatusMPRV && !mmuIO.fetch, io.mstatusMPP, io.priv)
  val bare = !io.satp(31) || priv(1) // M-mode不进行地址转换

  // TLB读取
  val tlb = Module(new Tlb)
  val hit = tlb.readIO.valid
  tlb.io.flush := io.fenceVMA
  tlb.readIO.vpn := mmuIO.vpn
  val res = tlb.readIO
  val resPpn = Mux(res.mega, Cat(res.ppn(19, 10), mmuIO.vpn(9, 0)), res.ppn)
  val privPF = Mux(priv(0), res.user && (mmuIO.fetch || !io.mstatusSUM), !res.user)
  val actPF = (mmuIO.fetch && res.exec) || (mmuIO.store && res.write) ||
    (mmuIO.load && (res.read || (res.exec && io.mstatusMXR)))
  val flagPF = !res.access || (!res.dirty && mmuIO.store)

  // TLB缺失，请求PTW
  val ptwIO = IO(MmuBundle.walkerIO)
  val ptwReq = RegInit(false.B)
  ptwIO.req := ptwReq
  ptwIO.ptn := io.satp(19, 0)
  ptwIO.vpn := mmuIO.vpn
  when (mmuIO.valid && !bare && !hit && !io.flush) { ptwReq := true.B }
  when (ptwIO.valid) { ptwReq := false.B }
  tlb.writeIO.value := ptwIO.value
  tlb.writeIO.vpn := mmuIO.vpn

  // 翻译结果
  mmuIO.hit := bare || hit
  mmuIO.pf := !bare && (privPF || actPF || flagPF)
  mmuIO.cause := 12.U(4.W) | Cat(0.U(2.W), mmuIO.store, !mmuIO.fetch)
  mmuIO.ppn := Mux(bare, mmuIO.vpn, resPpn)
}
