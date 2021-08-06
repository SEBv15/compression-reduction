package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Find the location of the first 1 from the left in the input bits
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The number of bits
 */
class FirstOne(length: Int) extends Module {
    require(length > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(length, Bool()))
        val out = Output(UInt(log2Ceil(length + 1).W))
    })

    if (length == 1) {
        io.out := ~io.in(0)
    } else {
        val length0 = if (isPow2(length)) length >> 1 else 1 << log2Floor(length)
        val length1 = length - length0

        val fo0 = Module(new FirstOne(length0)).io
        val fo1 = Module(new FirstOne(length1)).io

        fo0.in := io.in.slice(0, length0)
        fo1.in := io.in.slice(length0, length)

        when (fo0.out(fo0.out.getWidth - 1) === 0.U) {
            io.out := fo0.out
        }.otherwise {
            io.out := (1 << log2Ceil(length) - 1).U +& fo1.out
        }
    }

    override def desiredName = s"FirstOne_${length}"
}

object FirstOne extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new FirstOne(length = 16)))
    )
}