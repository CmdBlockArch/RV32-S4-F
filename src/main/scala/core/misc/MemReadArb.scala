package core.misc

import chisel3._
import perip.AxiReadIO

class MemReadIO extends Bundle {
  val req = Output(Bool())
  val addr = Output(UInt(32.W))
  val size = Output(UInt(2.W))
  val burst = Output(UInt(2.W))
  val len = Output(UInt(8.W))

  val resp = Input(Bool())
  val data = Input(UInt(32.W))
  val err = Input(Bool())
  val last = Input(Bool())

  def setBurst(n: Int = 1) = {
    size := "b10".U // 4 bytes
    if (n == 1) {
      burst := "b00".U // single
    } else {
      burst := "b10".U // wrap
    }
    len := (n - 1).U
  }
}

class MemReadArb(nrMaster: Int) extends Module {
  val master = IO(Vec(nrMaster, Flipped(new MemReadIO)))
  val slave = IO(new AxiReadIO)

  // 每个master是否正在进行请求
  val running = RegInit(VecInit(Seq.fill(nrMaster)(false.B)))
  // 每个master是否有待接收的请求
  val pending = Wire(Vec(nrMaster, Bool()))
  for (i <- 0 until nrMaster) pending(i) := !running(i) && master(i).req
  // 当前是否存在待接收的请求
  val doReq = WireDefault(pending.reduce(_ || _))
  // 若有请求待接收，应该是哪个id（id大优先级更高）
  val doReqId = Wire(UInt(4.W))
  doReqId := (0 until nrMaster).foldLeft(0.U)((res, i) => Mux(pending(i), i.U, res))

  // 向slave发送请求
  val curValid = RegInit(false.B) // 当前请求是否有效
  val curId = Reg(UInt(4.W)) // 当前请求的id
  val curMaster = master(curId)
  slave.arvalid := curValid
  slave.arid    := curId
  slave.araddr  := curMaster.addr
  slave.arsize  := curMaster.size
  slave.arburst := curMaster.burst
  slave.arlen   := curMaster.len
  // 有待发送的请求，且可以发送
  when (doReq && (!curValid || slave.arready)) {
    // 发送请求
    curValid := true.B
    curId := doReqId
    // 将请求标记为正在进行
    running(doReqId) := true.B
  } .elsewhen (curValid && slave.arready) {
    // 当前请求被取走，且没有新的请求到来
    curValid := false.B
  }

  // 将slave返回转发到各个master
  slave.rready := true.B // 始终可以接收返回
  for (i <- 0 until nrMaster) {
    // 是否当前返回给特定的slave
    val sel = WireDefault(slave.rid === i.U)
    master(i).resp := sel && slave.rvalid
    // 只在resp为高时，以下信号有效
    master(i).last := slave.rlast
    master(i).data := slave.rdata
    master(i).err := slave.rresp.orR
    // 当请求结束时，清除running标记
    when (master(i).resp && master(i).last) {
      running(i) := false.B
    }
  }

}
