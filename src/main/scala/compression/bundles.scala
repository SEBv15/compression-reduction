package compression

import chisel3._
import chisel3.util._

// Bundle to represent a variable sized amount of data
class DynamicData(val maxsize: Int, val elemsize: Int = 16) extends Bundle {
    val data = Vec(maxsize, UInt(elemsize.W))
    val len  = UInt(log2Ceil(maxsize + 1).W)
}
