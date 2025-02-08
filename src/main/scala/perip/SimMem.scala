package perip

import chisel3._
import chisel3.util._
import chisel3.util.circt.dpi.{DPINonVoidFunctionImport, DPIClockedVoidFunctionImport}
import utils.MuxLookup1H

object BurstAddrNext {
  def apply(addr: UInt, len: UInt, size: UInt, burst: UInt): UInt = {
    // burst len在mask中占的位数
    val lenW = Wire(UInt(3.W))
    lenW := MuxLookup1H(len)(Seq(
      0x0.U -> 0.U,
      0x1.U -> 1.U,
      0x3.U -> 2.U,
      0x7.U -> 3.U,
      0xf.U -> 4.U,
    ))
    // wrap模式的mask
    val wrapMask = WireDefault(UInt(32.W), (1.U << (size +& lenW)) - 1.U)
    // 只考虑增加，下一次传输的地址
    val addrIncr = WireDefault(UInt(32.W), addr + (1.U << size))
    // 下一次传输的地址，考虑不同的burst模式
    MuxLookup1H(burst)(Seq(
      "b00".U -> addr,
      "b01".U -> addrIncr,
      "b10".U -> ((addr & ~wrapMask) | (addrIncr & wrapMask)),
    ))
  }
}

class SimMemRead extends Module {
  val io = IO(Flipped(new AxiReadIO))

  class ReqBundle extends Bundle {
    val araddr  = UInt(32.W)
    val arid    = UInt(4.W)
    val arlen   = UInt(8.W)
    val arsize  = UInt(3.W)
    val arburst = UInt(2.W)
  }

  // 读延迟
  val delay = 0 // 31
  val counter = if (delay > 0) RegInit(0.U(log2Up(delay).W)) else 0.U(1.W)
  if (delay > 0) {
    when (counter =/= delay.U) { counter := counter + 1.U }
    when (io.arready && io.arvalid) { counter := 0.U }
  }

  // cur：当前正在处理的读请求
  val cur = Reg(new ReqBundle)
  val curValid = RegInit(false.B)
  val curCnt   = Reg(UInt(8.W))
  val curLast  = WireDefault(Bool(), curCnt === 0.U)
  val curData  = Wire(UInt(32.W))
  // 将cur接到axi的r返回通道
  io.rvalid := curValid && counter === delay.U
  io.rresp := 0.U
  io.rdata := curData
  io.rlast := curLast
  io.rid := cur.arid
  // 当前没有正在处理的读请求，则可以接收下一个请求
  io.arready := !curValid

  // 读取物理内存DPI-C调用
  object PmemRead extends DPINonVoidFunctionImport[UInt] {
    override val functionName = "pmem_read"
    override val ret = UInt(32.W)
    override val clocked = true
    override val inputNames = Some(Seq("raddr", "rsize"))
    override val outputName = Some("rdata")
  }
  val dpiReadAddr = WireDefault(UInt(32.W), DontCare)
  val dpiReadSize = WireDefault(UInt(32.W), DontCare)
  val dpiReadEnable = WireDefault(false.B)
  curData := PmemRead.callWithEnable(dpiReadEnable, dpiReadAddr, dpiReadSize)

  // 生成下一次burst读取的地址
  val addrNext = WireDefault(UInt(32.W), BurstAddrNext(cur.araddr, cur.arlen, cur.arsize, cur.arburst))

  when (io.arready && io.arvalid) { // 接收请求进行处理，生成第一个返回
    cur := io
    curCnt := io.arlen
    curValid := true.B
    // curData <= PmemRead(reqQueueOut.bits.addr)
    dpiReadAddr := io.araddr
    dpiReadSize := io.arsize
    dpiReadEnable := true.B
  } .elsewhen (io.rready && io.rvalid) { // 当前返回被接收
    when (curLast) { // 这是最后一个返回，当前请求处理完毕
      curValid := false.B
    } .otherwise { // 产生下一个返回
      curCnt := curCnt - 1.U
      cur.araddr := addrNext
      // curData <= PmemRead(addrNext)
      dpiReadAddr := addrNext
      dpiReadSize := cur.arsize
      dpiReadEnable := true.B
    }
  }
}

class SimMemWrite extends Module {
  val io = IO(Flipped(new AxiWriteIO))

  class ReqBundle extends Bundle {
    val awaddr  = Output(UInt(32.W))
    val awid    = Output(UInt(4.W))
    val awlen   = Output(UInt(8.W))
    val awsize  = Output(UInt(3.W))
    val awburst = Output(UInt(2.W))
  }

  // 写延迟
  val delay = 0 // 31
  val counter = if (delay > 0) RegInit(0.U(log2Up(delay).W)) else 0.U(1.W)
  if (delay > 0) {
    when (counter =/= delay.U) { counter := counter + 1.U }
    when (io.awvalid && io.awready) { counter := 0.U }
  }

  // cur：当前正在处理的写请求
  val cur = Reg(new ReqBundle)
  val curValid = RegInit(false.B)
  val curResp = RegInit(false.B)
  // 当前没有正在处理的写请求，则可以接收下一个请求
  io.awready := !curValid
  // 接收地址后，传输完成前，可以接收数据
  io.wready := curValid && !curResp
  // b返回通道输出
  io.bvalid := curResp && counter === delay.U
  io.bresp := 0.U
  io.bid := cur.awid

  when (io.awready && io.awvalid) { // 接收写请求，保存地址等信息
    cur := io
    curValid := true.B
  } .elsewhen (io.bready && io.bvalid) { // 返回被接收
    curResp := false.B
    curValid := false.B
  }

  // 写入物理内存DPI-C调用
  object PmemWrite extends DPIClockedVoidFunctionImport {
    override val functionName = "pmem_write"
    override val inputNames = Some(Seq("waddr", "wsize", "wdata", "wmask"))
  }
  // 生成下一次burst写入的地址
  val addrNext = WireDefault(UInt(32.W), BurstAddrNext(cur.awaddr, cur.awlen, cur.awsize, cur.awburst))
  // 收到写入的数据
  when (io.wready && io.wvalid) {
    PmemWrite.call(cur.awaddr, Cat(0.U(29.W), cur.awsize), io.wdata, Cat(0.U(4.W), io.wstrb))
    when (io.wlast) { // burst请求完成s
      curResp := true.B
    } .otherwise {
      cur.awaddr := addrNext
    }
  }
}
