package core.decode

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode._
import OpcodeRawStr._

object valASelField extends DecodeField[InstPattern, UInt] {
  override def name = "valASel"
  override def chiselType = UInt(3.W)
  override def genTable(op: InstPattern) = {
    op.opcode.rawString match {
      case LUI | STORE        => BitPat("b001") // 0
      case AUIPC | JAL | JALR => BitPat("b010") // pc
      case CALRI | CALRR      => BitPat("b100") // src1
      case SYSTEM => op.func3.rawString match {
        case "000" => dc
        case _ => BitPat("b001") // zicsr: 0
      }
      case ATOMIC => op.func5.rawString match {
        case "00010" => dc // lr
        case _ => BitPat("b001") // sc,amo: 0
      }
      case _ => dc
    }
  }
}

object ValBSelField extends DecodeField[InstPattern, UInt] {
  override def name = "valBSel"
  override def chiselType = UInt(5.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case JAL | JALR    => BitPat("b00001") // 4
      case LUI | AUIPC   => BitPat("b00010") // immU
      case CALRI         => BitPat("b00100") // immI
      case STORE | CALRR => BitPat("b01000") // src2
      case SYSTEM => op.func3.rawString match {
        case "000" => dc
        case _ => BitPat("b10000") // zicsr: csrVal
      }
      case ATOMIC => op.func5.rawString match {
        case "00010" => dc // lr
        case _ => BitPat("b01000") // sc,amo: src2
      }
      case _ => dc
    }
  }
}

object ValCSelField extends DecodeField[InstPattern, UInt] {
  override def name = "valCSel"
  override def chiselType = UInt(3.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case LOAD | STORE => BitPat("b001") // src1+imm（用mem选择立即数类型）
      case SYSTEM => op.func3.rawString match {
        case "000" => dc
        case s if s(0) == '0' => BitPat("b010") // CSRR: src1
        case s if s(0) == '1' => BitPat("b100") // CSRI: immZ
      }
      case ATOMIC => BitPat("b010") // src1
      case _ => dc
    }
  }
}

object FuncEnField extends BoolDecodeField[InstPattern] {
  // 1: func = func3 + funcs
  // 0: func = 0
  override def name = "funcEn"
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case CALRI | CALRR => y
      case LUI | AUIPC | JAL | JALR | STORE => n
      case SYSTEM => op.func3.rawString match {
        case "000" => dc
        case _ => n // zicsr
      }
      case ATOMIC => op.func5.rawString match {
        case "00010" => dc // lr
        case _ => n // sc, amo
      }
      case _ => dc
    }
  }
}

object MulField extends BoolDecodeField[InstPattern] {
  override def name = "mul" // 是否为乘法操作
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case CALRR => op.func7.rawString match {
        case "0000001" => y // mul
        case _ => n
      }
      case _ => n
    }
  }
}

object RdEnField extends BoolDecodeField[InstPattern] {
  override def name = "rdEn" // 是否有寄存器写入
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case LUI | AUIPC | JAL | JALR | LOAD | CALRI | CALRR | ATOMIC => y
      case BRANCH | STORE => n
      case SYSTEM => op.func3.rawString match {
        case "000" => dc // rd位均为0
        case _ => y // zicsr
      }
      case _ => n
    }
  }
}

object MemField extends DecodeField[InstPattern, UInt] {
  override def name = "mem" // 访存操作编码
  override def chiselType = UInt(4.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case LOAD => op.func3.rawString match {
        case "000" => BitPat("b1100") // lb
        case "001" => BitPat("b1101") // lh
        case "010" => BitPat("b1110") // lw
        case "100" => BitPat("b1000") // lbu
        case "101" => BitPat("b1001") // lhu
        case _ => dc
      }
      case STORE  => op.func3.rawString match {
        case "000" => BitPat("b0100") // sb
        case "001" => BitPat("b0101") // sh
        case "010" => BitPat("b0110") // sw
        case _ => dc
      }
      case ATOMIC => op.func5.rawString match {
        case "00010" => BitPat("b0001") // lr
        case "00011" => BitPat("b0010") // sc
        case _       => BitPat("b0011") // amo
      }
      case _ => BitPat("b0000") // 0
    }
  }
}

object ZicsrEnField extends BoolDecodeField[InstPattern] {
  override def name = "zicsrEn" // 是否为CSR操作，若是传递func3[1:0]
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case SYSTEM => op.func3.rawString match {
        case "000" => n
        case _ => y // zicsr
      }
      case _ => n
    }
  }
}

object RetField extends DecodeField[InstPattern, UInt] {
  override def name = "ret" // RET操作两位操作码（无:00, S:01, M:11）
  override def chiselType = UInt(2.W)
  override def genTable(op: InstPattern): BitPat = {
    if      (InstPattern.mret.bitPat.cover(op.inst)) BitPat("b11")
    else if (InstPattern.sret.bitPat.cover(op.inst)) BitPat("b01")
    else BitPat("b00")
  }
}

object fenceIField extends BoolDecodeField[InstPattern] {
  override def name = "fenceI" // 是否为FENCE.I操作
  override def genTable(op: InstPattern): BitPat = {
    if (InstPattern.fencei.bitPat.cover(op.inst)) y else n
  }
}

object fenceVMAField extends BoolDecodeField[InstPattern] {
  override def name = "fenceVMA" // 是否为FENCE.VMA操作
  override def genTable(op: InstPattern): BitPat = {
    if (InstPattern.sfcvma.bitPat.cover(op.inst)) y else n
  }
}

object InvInstField extends BoolDecodeField[InstPattern] {
  override def name = "invInst" // 是否为非法指令
  override def default = y
  override def genTable(op: InstPattern): BitPat = n
}

object JmpField extends DecodeField[InstPattern, UInt] {
  override def name = "jmp" // 跳转类型
  override def chiselType = UInt(3.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case JAL => BitPat("b001")
      case JALR => BitPat("b010")
      case BRANCH => BitPat("b100")
      case _ => dc
    }
  }
}

object InstField {
  val fields = Seq(
    valASelField,
    ValBSelField,
    ValCSelField,
    FuncEnField,
    MulField,
    RdEnField,
    MemField,
    ZicsrEnField,
    RetField,
    fenceIField,
    fenceVMAField,
    InvInstField,
    JmpField,
  )
}
