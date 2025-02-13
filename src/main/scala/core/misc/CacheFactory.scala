package core.misc

import chisel3._
import utils.SRam

class CacheFactory(val offsetW: Int = 4, val indexW: Int = 4, val wayW: Int = 1) {
  val wayN = 1 << wayW
  val plruW = wayN - 1

  // Tag | Index | Offset
  val tagW = 32 - offsetW - indexW // tag宽度

  val setN = 1 << indexW // 组数
  val blockN = 1 << (offsetW - 2) // 每块中，32位的字的数量
  val blockW = 1 << (offsetW + 3) // 块大小，单位bit

  val validType = Vec(wayN, Bool())
  val tagType = Vec(wayN, UInt(tagW.W))
  val wayDataType = Vec(blockN, UInt(32.W))
  val dataType = Vec(wayN, wayDataType)
  val plruType = UInt(plruW.W)

  def getOffset(addr: UInt): UInt = addr(offsetW - 1, 2)
  def getIndex(addr: UInt): UInt = addr(indexW + offsetW - 1, offsetW)
  def getTag(addr: UInt): UInt = addr(31, indexW + offsetW)

  def tagHit(cvalid: Vec[Bool], ctag: Vec[UInt], tag: UInt) = {
    val hitVec = VecInit(cvalid.zip(ctag).map{
      case (v, t) => v && t === tag
    })
    val hit = hitVec.reduce(_ || _)
    val way = hitVec.onlyIndexWhere(_.asBool)
    (hit, way)
  }

  class ReadIO extends Bundle {
    val index = Output(UInt(indexW.W))

    val valid = Input(validType)
    val tag = Input(tagType)
    val data = Input(dataType)
    val plru = Input(plruType)
  }

  class WriteIO extends Bundle {
    val en = Output(Bool())
    val index = Output(UInt(indexW.W))

    val valid = Output(validType)
    val tag = Output(tagType)
    val data = Output(dataType)
    val plru = Output(plruType)
  }

  object WriteIO {
    def initGen = {
      val w = Wire(new WriteIO)
      w := DontCare
      w.en := false.B
      w
    }
  }

  class Cache extends Module {
    val readIO = IO(Flipped(new ReadIO))
    val writeIO = IO(Flipped(new WriteIO))

    val valid = RegInit(VecInit(Seq.fill(setN)(VecInit(Seq.fill(wayN)(false.B)))))
    val plru = Reg(Vec(setN, UInt(plruW.W)))
    val tag = Seq.fill(wayN)(Module(new SRam(indexW, tagW)))
    val data = Seq.fill(wayN)(Module(new SRam(indexW, blockW)))

    // 读端口
    val forward = WireDefault(writeIO.en && readIO.index === writeIO.index)
    when (forward) {
      readIO.valid := writeIO.valid
      readIO.plru := writeIO.plru
    } .otherwise {
      readIO.valid := valid(readIO.index)
      readIO.plru := plru(readIO.index)
    }

    tag.foreach(_.readIO.addr := readIO.index)
    readIO.tag := tag.map(_.readIO.data)

    data.foreach(_.readIO.addr := readIO.index)
    readIO.data := data.map(_.readIO.data.asTypeOf(wayDataType))

    // 写端口
    when (writeIO.en) {
      valid(writeIO.index) := writeIO.valid
      plru(writeIO.index) := writeIO.plru
    }
    tag.foreach(_.writeIO.en := writeIO.en)
    tag.foreach(_.writeIO.addr := writeIO.index)
    tag.zip(writeIO.tag).foreach{case (t, w) => t.writeIO.data := w}
    data.foreach(_.writeIO.en := writeIO.en)
    data.foreach(_.writeIO.addr := writeIO.index)
    data.zip(writeIO.data).foreach{case (d, w) => d.writeIO.data := w.asUInt}

    // 冲刷
    val io = IO(new Bundle {
      val flush = Input(Bool())
    })
    when (io.flush) {
      valid.foreach(_ := 0.U.asTypeOf(validType))
    }
  }
}
