package core.misc

import chisel3._
import utils.SRam

class CacheFactory(val offsetW: Int = 4, val indexW: Int = 4, val wayW: Int = 1) {
  val wayN = 1 << wayW

  // Tag | Index | Offset
  val tagW = 32 - offsetW - indexW // tag宽度

  val setN = 1 << indexW // 组数
  val blockN = 1 << (offsetW - 2) // 每块中，32位的字的数量
  val blockW = 1 << (offsetW + 3) // 块大小，单位bit

  val validType = Vec(wayN, Bool())
  val tagType = Vec(wayN, UInt(tagW.W))
  val wayDataType = Vec(blockN, UInt(32.W))
  val dataType = Vec(wayN, wayDataType)
  val plruType = Vec(setN, UInt((wayN - 1).W))

  def getOffset(addr: UInt): UInt = addr(offsetW - 1, 2)
  def getIndex(addr: UInt): UInt = addr(indexW + offsetW - 1, offsetW)
  def getTag(addr: UInt): UInt = addr(31, indexW + offsetW)

  def tagHitVec(cvalid: Vec[Bool], ctag: Vec[UInt], tag: UInt) = {
    VecInit(cvalid.zip(ctag).map{
      case (v, t) => v && t === tag
    })
  }

  class ReadIO extends Bundle {
    val index = Output(UInt(indexW.W))

    val valid = Input(validType)
    val tag = Input(tagType)
    val data = Input(dataType)
  }

  class WriteIO extends Bundle {
    val valid = Output(Bool())
    val index = Output(UInt(indexW.W))
    val way = Output(UInt(wayW.W))

    val tag = Output(UInt(tagW.W))
    val data = Output(wayDataType)
  }

  object WriteIO {
    def initGen = {
      val w = Wire(new WriteIO)
      w := DontCare
      w.valid := false.B
      w
    }
  }

  class Cache extends Module {
    val readIO = IO(Flipped(new ReadIO))
    val writeIO = IO(Flipped(new WriteIO))

    val valid = RegInit(VecInit(Seq.fill(setN)(VecInit(Seq.fill(wayN)(false.B)))))
    val tag = Seq.fill(wayN)(Module(new SRam(indexW, tagW)))
    val data = Seq.fill(wayN)(Module(new SRam(indexW, blockW)))

    // 读端口
    val forward = WireDefault(writeIO.valid && readIO.index === writeIO.index)
    readIO.valid := valid(readIO.index)
    when (forward) {
      readIO.valid(writeIO.way) := writeIO.valid
    }
    tag.foreach(_.readIO.addr := readIO.index)
    readIO.tag := tag.map(_.readIO.data)
    data.foreach(_.readIO.addr := readIO.index)
    readIO.data := data.map(_.readIO.data.asTypeOf(wayDataType))

    // 写端口
    when (writeIO.valid) {
      valid(writeIO.index)(writeIO.way) := writeIO.valid
    }
    def setWriteEn(s: Seq[SRam]) = {
      (0 until wayN).zip(s).foreach {
        case (i, t) => t.writeIO.en := writeIO.valid && writeIO.way === i.U
      }
    }
    setWriteEn(tag)
    tag.foreach(_.writeIO.addr := writeIO.index)
    tag.foreach(_.writeIO.data := writeIO.tag)
    setWriteEn(data)
    data.foreach(_.writeIO.addr := writeIO.index)
    data.foreach(_.writeIO.data := writeIO.data.asUInt)

    // 冲刷
    val io = IO(new Bundle {
      val flush = Input(Bool())
    })
    when (io.flush) {
      valid.foreach(_ := 0.U.asTypeOf(validType))
    }
  }
}
