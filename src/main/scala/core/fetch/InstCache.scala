package core.fetch

import chisel3._
import utils.SRam

object InstCacheUtils {
  // Tag | Index | Offset
  val offsetW = 4 // 块内地址宽度，块大小2^x字节
  val indexW = 2 // 组地址宽度，组数2^x
  val tagW = 32 - offsetW - indexW // tag宽度

  val setN = 1 << indexW // 组数
  val blockN = 1 << (offsetW - 2) // 每块指令数
  val blockW = (1 << offsetW) << 3 // 块大小，单位bit

  val dataType = Vec(blockN, UInt(32.W))

  def getOffset(addr: UInt): UInt = addr(offsetW - 1, 2)
  def getIndex(addr: UInt): UInt = addr(indexW + offsetW - 1, offsetW)
  def getTag(addr: UInt): UInt = addr(31, indexW + offsetW)
}

// 一路指令缓存（直接相连指令缓存）
class InstCacheWay extends Module {
  import InstCacheUtils._

  val valid = RegInit(VecInit(Seq.fill(setN)(false.B)))
  val tag = Module(new SRam(indexW, tagW))
  val data = Module(new SRam(indexW, blockW))

  // 读端口
  val readIO = IO(new Bundle {
    val index = Input(UInt(indexW.W))
    val valid = Output(Bool())
    val tag = Output(UInt(tagW.W))
    val data = Output(dataType)
  })
  readIO.valid := valid(readIO.index)
  tag.readIO.addr := readIO.index
  readIO.tag := tag.readIO.data
  data.readIO.addr := readIO.index
  readIO.data := data.readIO.data.asTypeOf(dataType)

  // 写端口
  val writeIO = IO(new Bundle {
    val en = Input(Bool())
    val index = Input(UInt(indexW.W))
    val tag = Input(UInt(tagW.W))
    val data = Input(dataType)
  })
  when (writeIO.en) {
    valid(writeIO.index) := true.B
  }
  tag.writeIO.en := writeIO.en
  tag.writeIO.addr := writeIO.index
  tag.writeIO.data := writeIO.tag
  data.writeIO.en := writeIO.en
  data.writeIO.addr := writeIO.index
  data.writeIO.data := writeIO.data.asUInt

  // 冲刷
  val io = IO(new Bundle {
    val flush = Input(Bool())
  })
  when (io.flush) {
    valid.foreach(_ := false.B)
  }
}

// TODO: 多路指令缓存
