package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Count the number of ones in the input bool vec
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The number of bools in the input Vec
 */
class CountOnes(length: Int) extends Module {
    require(length > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(length, Bool()))
        val out = Output(UInt(log2Ceil(length + 1).W))
    })

    if (length == 1) {
        io.out := io.in(0)
    } else {
        val length0 = if (isPow2(length)) length >> 1 else 1 << log2Floor(length)
        val length1 = length - length0

        val co0 = Module(new CountOnes(length0)).io
        val co1 = Module(new CountOnes(length1)).io

        co0.in := io.in.slice(0, length0)
        co1.in := io.in.slice(length0, length)

        io.out := co0.out +& co1.out
    }

    override def desiredName = s"CountOnes_${length}"
}

object CountOnes extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new CountOnes(length = 16)))
    )
}