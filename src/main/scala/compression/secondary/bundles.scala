package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

class LZ77Encoding(val posbits: Int, val lenbits: Int) extends Bundle {
    val pos = UInt(posbits.W)
    val len = UInt(lenbits.W)
}