package core.decode

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode._
import OpcodeRawStr._

object ImmSelField extends DecodeField[InstPattern, UInt] {
  override def name = "immSel"
  override def chiselType = UInt(6.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case JALR | LOAD | CALRI => BitPat("b000001") // immI
      case STORE               => BitPat("b000010") // immS
      case BRANCH              => BitPat("b000100") // immB
      case LUI | AUIPC         => BitPat("b001000") // immU
      case JAL                 => BitPat("b010000") // immJ
      case SYSTEM => op.func3.rawString match {
        case "000" => dc
        case _ => BitPat("b100000") // Zicsr, immZ
      }
      case _ => dc
    }
  }
}

object ValASelField extends DecodeField[InstPattern, UInt] {
  override def name = "valASel"
  override def chiselType = UInt(3.W)
  override def genTable(op: InstPattern) = {
    op.opcode.rawString match {
      case LUI => BitPat("b001") // 0
      case AUIPC | JAL | JALR => BitPat("b010") // pc
      case LOAD | STORE | CALRI | CALRR => BitPat("b100") // src1
      case _ => dc
    }
  }
}

object ValBSelField extends DecodeField[InstPattern, UInt] {
  override def name = "valBSel"
  override def chiselType = UInt(3.W)
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case JAL | JALR => BitPat("b001") // 4
      case LUI | AUIPC | LOAD | STORE | CALRI => BitPat("b010") // imm
      case CALRR => BitPat("b100") // src2
      case _ => dc
    }
  }
}

object AluFuncEnField extends BoolDecodeField[InstPattern] {
  override def name = "aluFuncEn"
  // 1: func = func3 + funcs
  // 0: func = 0
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case CALRI | CALRR | BRANCH => y // 分支func复用
      case LUI | AUIPC | JAL | JALR | LOAD | STORE => n
      case _ => dc
    }
  }
}

object AluSignEnField extends BoolDecodeField[InstPattern] {
  override def name = "aluSignEn" // 是否为有符号操作
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case CALRI => op.func3.rawString match {
        case "101" => y // srai
        case "001" => dc // slli, srli
        case _ => n
      }
      case CALRR => op.func3.rawString match {
        case "101" | "000" => y // sra, sub
        case _ => dc
      }
      case LUI | AUIPC | JAL | JALR | LOAD | STORE => n
      case _ => dc
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

object FwReadyField extends BoolDecodeField[InstPattern] {
  override def name = "fwReady" // Exec阶段是否可以直接前递
  override def genTable(op: InstPattern): BitPat = {
    op.opcode.rawString match {
      case LUI | AUIPC | JAL | JALR | CALRI => y
      case CALRR => op.func7.rawString match {
        case "0000001" => n // 乘法无法直接前递
        case _ => y
      }
      case _ => n
    }
  }
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

object FenceIField extends BoolDecodeField[InstPattern] {
  override def name = "fenceI" // 是否为FENCE.I操作
  override def genTable(op: InstPattern): BitPat = {
    if (InstPattern.fencei.bitPat.cover(op.inst)) y else n
  }
}

object FenceVMAField extends BoolDecodeField[InstPattern] {
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



object InstField {
  val fields = Seq(
    ImmSelField, ValASelField, ValBSelField, AluFuncEnField, AluSignEnField,
    RdEnField, FwReadyField, JmpField, MulField, MemField, ZicsrEnField,
    RetField, FenceIField, FenceVMAField, InvInstField
  )
}
