package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Purely combinational compression and reduction circuit that takes in the pixels, compresses them, and puts them into one continuous vector (with optional poisson encoding)
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReductionNoPacking(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val bits_per_pixel:Int = 10, val maxblocks:Int = 128) extends Module {
    require(isPow2(pixel_rows))
    require(isPow2(pixel_cols))
    require(pixel_cols == 8)
    require(pixel_rows >= 16)

    val big_one: BigInt = 1
    val outwords = (pixel_rows/2*6+pixel_rows*8*bits_per_pixel)/16

    val io = IO(new Bundle {
        // The raw 10-bit pixel data from a single shift (128x8 pixels).
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(bits_per_pixel.W))))

        // Whether to use poisson encoding
        val poisson = Input(Bool())

        // When compressing, this will be the compressed data from multiple shifts merged together, formatted into 1024-bit words to fit the FIFO.
        // Otherwise it will be the raw pixel data row by row.
        val out = Output(new DynamicData(outwords, elemsize = 16))
    })

    val encoders = Module(new Encoders(pixel_rows, pixel_cols))
    encoders.io.pixels := io.pixels
    encoders.io.poisson := io.poisson

    // Pass the compressed pixels through the reduction stage
    val reducer = Module(new HierarchicalReduction(pixel_rows/2, bits_per_pixel, 16, maxblocks))
    for (i <- 0 until pixel_rows/2) {
        reducer.io.in(i) := encoders.io.out(i)
    }

    io.out := reducer.io.out
}

object CompressionReductionNoPacking extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new CompressionReductionNoPacking))
    )
}
