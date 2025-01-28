package core.gpr

import chisel3._

class GprReadIO extends Bundle {
  val rs1 = Output(UInt(5.W))
  val rs2 = Output(UInt(5.W))
  val src1Ready = Input(Bool())
  val src1 = Input(UInt(32.W))
  val src2Ready = Input(Bool())
  val src2 = Input(UInt(32.W))
}

class GprWriteIO extends Bundle {
  val en = Output(Bool())
  val rd = Output(UInt(5.W))
  val data = Output(UInt(32.W))
}

class GprFwIO extends Bundle {
  val valid = Output(Bool()) // 当前流水级是否有效
  val rd = Output(UInt(5.W))
  val ready = Output(Bool()) // 转发值是否准备好
  val fwVal = Output(UInt(32.W))
}

class RegFile extends Module {
  val readIO = IO(Flipped(new GprReadIO))
  val writeIO = IO(Flipped(new GprWriteIO))

  val fwIO = IO(Flipped(Vec(3, new GprFwIO))) // id小的流水级优先级高，位于上游

  val regs = Reg(Vec(32, UInt(32.W)))

  def readGpr(rs: UInt): (Bool, UInt) = {
    val ready = Wire(Bool())
    val src = Wire(UInt(32.W))

    var ctx = when (rs === 0.U(5.W)) {
      ready := true.B
      src := 0.U(32.W)
    }

    fwIO.foreach(fwIO => {
      ctx = ctx.elsewhen (fwIO.valid && fwIO.rd === rs) {
        ready := fwIO.ready
        src := fwIO.fwVal
      }
    })

    ctx.otherwise {
      ready := true.B
      src := regs(rs)
    }

    (ready, src)
  }

  val res1 = readGpr(readIO.rs1)
  readIO.src1Ready := res1._1
  readIO.src1 := res1._2
  val res2 = readGpr(readIO.rs2)
  readIO.src2Ready := res2._1
  readIO.src2 := res2._2

  when(writeIO.en && writeIO.rd.orR) {
    regs(writeIO.rd) := writeIO.data
  }

  val debugOut = IO(Output(Vec(32, UInt(32.W))))
  debugOut := regs
}
