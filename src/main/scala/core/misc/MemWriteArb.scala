package core.misc

import chisel3._
import perip.AxiWriteIO

class MemWriteIO extends Bundle {
  val req = Output(Bool())
  val addr = Output(UInt(32.W))
  val size = Output(UInt(2.W))
  val burst = Output(UInt(2.W))
  val len = Output(UInt(8.W))

  val dataReady = Input(Bool())
  val dataValid = Output(Bool())
  val data = Output(UInt(32.W))
  val strb = Output(UInt(4.W))
  val last = Output(Bool())

  val resp = Input(Bool())
  val err = Input(Bool())
}

class MemWriteArb(nrMaster: Int) extends Module {
  val master = IO(Vec(nrMaster, Flipped(new MemWriteIO)))
  val slave = IO(new AxiWriteIO)

  // 当前是否存在待接收的请求
  val doReq = WireDefault(master.foldLeft(false.B)((res, m) => res || m.req))
  // 若有请求待接收，应该是哪个id（id大优先级更高）
  val doReqId = Wire(UInt(4.W))
  doReqId := (0 until nrMaster).foldLeft(0.U)((res, i) => Mux(master(i).req, i.U, res))

  val running = RegInit(false.B) // 是否正在进行请求
  val curId = Reg(UInt(4.W)) // 当前请求id
  val awvalid = RegInit(false.B) // 向slave转发请求的awvalid
  val curMaster = master(curId)
  when (!running && doReq) { // 可以接受请求
    running := true.B
    awvalid := true.B
    curId := doReqId
  }

  // 向slave转发请求
  slave.awvalid := awvalid
  when (slave.awready && slave.awvalid) {
    awvalid := false.B
  }
  slave.awaddr := curMaster.addr
  slave.awid := curId
  slave.awlen := curMaster.len
  slave.awsize := curMaster.size
  slave.awburst := curMaster.burst

  // 始终准备好接收b通道返回
  slave.bready := true.B
  when (slave.bvalid) {
    running := false.B
  }

  for (i <- 0 until nrMaster) {
    // 转发b通道返回给每个master
    val sel = WireDefault(curId === i.U)
    master(i).resp := sel && slave.bvalid
    master(i).err := slave.bresp.orR

    master(i).dataReady := slave.wready
  }

  // 数据通道
  slave.wvalid := curMaster.dataValid
  slave.wdata := curMaster.data
  slave.wstrb := curMaster.strb
  slave.wlast := curMaster.last
}
