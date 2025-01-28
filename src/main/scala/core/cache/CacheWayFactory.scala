package core.cache

import chisel3._
import utils.SRam

class CacheWayFactory(val offsetW: Int = 5, val indexW: Int = 3) {
  // Tag | Index | Offset
  val tagW = 32 - offsetW - indexW // tag宽度

  val setN = 1 << indexW // 组数
  val blockN = 1 << (offsetW - 2) // 每块中，32位的字的数量
  val blockW = 1 << (offsetW + 3) // 块大小，单位bit

  val dataType = Vec(blockN, UInt(32.W))

  def getOffset(addr: UInt): UInt = addr(offsetW - 1, 2)
  def getIndex(addr: UInt): UInt = addr(indexW + offsetW - 1, offsetW)
  def getTag(addr: UInt): UInt = addr(31, indexW + offsetW)

  class readIO extends Bundle {
    val index = Output(UInt(indexW.W))
    val valid = Input(Bool())
    val dirty = Input(Bool())
    val tag = Input(UInt(tagW.W))
    val data = Input(dataType)
  }

  class writeIO extends Bundle {
    val en = Output(Bool())
    val index = Output(UInt(indexW.W))
    val dirty = Output(Bool())
    val tag = Output(UInt(tagW.W))
    val data = Output(dataType)
  }

  class CacheWay extends Module {
    val readIO = IO(Flipped(new readIO))
    val writeIO = IO(Flipped(new writeIO))

    val valid = RegInit(VecInit(Seq.fill(setN)(false.B)))
    val dirty = Reg(Vec(setN, Bool()))
    val tag = Module(new SRam(indexW, tagW))
    val data = Module(new SRam(indexW, blockW))

    // 读端口
    val forward = WireDefault(writeIO.en && readIO.index === writeIO.index)
    readIO.valid := Mux(forward, true.B, valid(readIO.index))
    readIO.dirty := Mux(forward, writeIO.dirty, dirty(readIO.index))

    tag.readIO.addr := readIO.index
    readIO.tag := tag.readIO.data

    data.readIO.addr := readIO.index
    readIO.data := data.readIO.data.asTypeOf(dataType)

    // 写端口
    when (writeIO.en) {
      valid(writeIO.index) := true.B
      dirty(writeIO.index) := writeIO.dirty
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
}
