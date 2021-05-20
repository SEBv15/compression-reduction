package compression
import chisel3._
import chisel3.util._

/** Chisel interface for the verilog Merge module.
 *  
 *  It is not possible to use sbt to generate verilog for, or test this module. 
 *  The wrapper module MergeVerilogWrapper has to be used instead.
 *  That module also has better accessibility for the vectors.
 *
 *  @author Sebastian Strempfer
 *
 *  @param width The bitwidth of the elements
 *  @param inwords1 Number of elements in the first vector
 *  @param inwords2 Number of elements in the second vector
 */
class MergeVerilog(val width: Int = 16, val inwords1:Int = 10, val inwords2:Int = 10) extends BlackBox(Map("WIDTH" -> width, "LEN1" -> inwords1, "LEN2" -> inwords2)) with HasBlackBoxResource {
    val outwords = inwords1+inwords2

    val io = IO(new Bundle {
        val data1 = Input(UInt((width*inwords1).W))
        val data2 = Input(UInt((width*inwords2).W))
        val len1 = Input(UInt((log2Floor(inwords1) + 1).W))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val outlen = Output(UInt((log2Floor(outwords) + 1).W))
        val out = Output(UInt((width*outwords).W))
    })
  
    setResource("/MergeVerilog.v")
}
