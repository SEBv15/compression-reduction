package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Lossy encode pixel value by adjusting the resolution based on the poisson noise.
 *
 *  @author Sebastian Strempfer
 *  @todo Parallelize length calculation (very slight time improvement)
 *
 *  @param npixels Number of pixels to compress together
 *  @param pixelsize Number of bits per pixel 
 */
class LengthCompress(val npixels:Int = 16, val pixelsize:Int = 10) extends Module {
    val io = IO(new Bundle {
        val in  = Input( Vec(npixels, UInt(pixelsize.W)))
        val data = Output(Vec(pixelsize, UInt(npixels.W)))
        val header = Output(UInt((log2Floor(pixelsize) + 1).W))
    })

    val shuffle = Module(new BitShufflePerChannel(npixels, pixelsize))

    // Shuffle the bits and output
    shuffle.io.in := io.in
    io.data := shuffle.io.out

    // There is probably a better way, but this works
    val lengths = Wire(Vec(pixelsize, UInt((log2Floor(pixelsize) + 1).W)))
    for (i <- 0 until pixelsize) {
        lengths(i) := Mux(shuffle.io.out(i) === 0.U, if (i > 0) lengths(i - 1) else {0.U}, (i+1).U)
    }
    io.header := lengths(pixelsize-1)
}

object LengthCompress extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new LengthCompress))
    )
}
