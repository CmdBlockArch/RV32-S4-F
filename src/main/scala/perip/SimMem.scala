package perip

import chisel3._
import chisel3.util._
import chisel3.util.circt.dpi.DPINonVoidFunctionImport
import utils._

// 读取物理内存DPI-C调用
object PmemRead extends DPINonVoidFunctionImport[UInt] {
  override val functionName = "pmem_read"
  override val ret = UInt(32.W)
  override val clocked = false
  override val inputNames = Some(Seq("raddr"))
  override val outputName = Some("rdata")
  final def apply(raddr: UInt): UInt = super.call(raddr)
}

class SimMemRead extends Module {
  val io = IO(Flipped(new AxiReadIO))

  // 读请求的所有信息bundle
  class ReqBundle extends Bundle {
    val addr  = UInt(32.W)
    val id    = UInt( 4.W)
    val len   = UInt( 8.W)
    val size  = UInt( 3.W)
    val burst = UInt( 2.W)
  }
  // 将读请求缓存在fifo中
  val reqQueue = Module(new Queue(new ReqBundle, 8, true, true))
  val reqQueueIn = reqQueue.io.enq
  val reqQueueOut = reqQueue.io.deq
  // 将axi的ar读请求通道接入fifo输入
  io.arready := reqQueue.io.enq.ready
  reqQueueIn.valid      := io.arvalid
  reqQueueIn.bits.addr  := io.araddr
  reqQueueIn.bits.id    := io.arid
  reqQueueIn.bits.len   := io.arlen
  reqQueueIn.bits.size  := io.arsize
  reqQueueIn.bits.burst := io.arburst

  // cur：当前正在处理的读请求
  val cur = Reg(new ReqBundle)
  val curValid = RegInit(false.B)
  val curLast = RegInit(false.B)
  val curData = Reg(UInt(32.W))
  // 将cur接到axi的r返回通道
  io.rvalid := curValid
  io.rresp := 0.U
  io.rdata := curData
  io.rlast := curLast
  io.rid := cur.id
  // 当前没有正在处理的读请求，或者当前请求已经处理完毕
  // 则可以从fifo中取出下一个请求
  reqQueueOut.ready := !curValid || (io.rready && curLast)

  // burst len在mask中占的位数
  val lenW = Wire(UInt(3.W))
  lenW := MuxLookup1H(cur.len)(Seq(
    0x0.U -> 0.U,
    0x1.U -> 1.U,
    0x3.U -> 2.U,
    0x7.U -> 3.U,
    0xf.U -> 4.U,
  ))
  // wrap模式的mask
  val wrapMask = WireInit(UInt(32.W), (1.U << (cur.size +& lenW)) - 1.U)
  // 只考虑增加，下一次传输的地址
  val addrIncr = WireInit(UInt(32.W), cur.addr + (1.U << cur.size))
  // 下一次传输的地址，考虑不同的burst模式
  val addrNext = Wire(UInt(32.W))
  addrNext := MuxLookup1H(cur.burst)(Seq(
    "b00".U -> cur.addr,
    "b01".U -> addrIncr,
    "b10".U -> ((cur.addr & ~wrapMask) | (addrIncr & wrapMask)),
  ))

  when (reqQueueOut.fire) { // 从队头取出一个请求进行处理，生成第一个返回
    cur := reqQueueOut.bits
    curValid := true.B
    curLast := reqQueueOut.bits.len === 0.U
    curData := PmemRead(reqQueueOut.bits.addr)
  } .elsewhen (io.rready && io.rvalid) { // 当前返回被接收
    when (curLast) { // 这是最后一个返回，当前请求处理完毕
      curValid := false.B
    } .otherwise { // 产生下一个返回
      cur.len := cur.len - 1.U
      curLast := cur.len === 1.U
      cur.addr := addrNext
      curData := PmemRead(addrNext)
    }
  }
}

class SimMemWrite extends Module {
  val io = IO(Flipped(new AxiWriteIO))
}
