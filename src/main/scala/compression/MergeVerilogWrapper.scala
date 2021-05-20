package compression
import chisel3._
import chisel3.util._
 
/** Wrapper for the interface for the verilog Merge module.
 *  
 *  @author Sebastian Strempfer
 *
 *  @param wordsize The bitwidth of the elements
 *  @param inwords1 Number of elements in the first vector
 *  @param inwords2 Number of elements in the second vector
 */
class MergeVerilogWrapper(val wordsize:Int = 16, val inwords1:Int = 10, val inwords2:Int = 10) extends Module {
    val outwords = inwords1+inwords2

    val io = IO(new Bundle {
        val len1 = Input(UInt((log2Floor(inwords1) + 1).W))
        val data1 = Input(Vec(inwords1, UInt(wordsize.W)))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val data2 = Input(Vec(inwords2, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(outwords) + 1).W))
        val out = Output(Vec(outwords, UInt(wordsize.W)))
    })

    val mergeVerilog = Module(new MergeVerilog(wordsize, inwords1, inwords2))
    mergeVerilog.io.len1 := io.len1
    mergeVerilog.io.len2 := io.len2
    io.outlen := mergeVerilog.io.outlen

    mergeVerilog.io.data1 := Cat(io.data1)
    mergeVerilog.io.data2 := Cat(io.data2)

    for (i <- 0 until outwords) {
        io.out(i) := mergeVerilog.io.out(wordsize*(outwords - i) - 1, wordsize*(outwords - i - 1))
    }
}

object MergeVerilogWrapper extends App {
    chisel3.Driver.execute(args, () => new MergeVerilogWrapper)
}
