package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Lossy encode pixel value by adjusting the resolution based on the poisson noise.
 *
 *  @author Sebastian Strempfer
 *
 *  @param npixels Number of pixels to compress together
 *  @param pixelsize Number of bits per pixel 
 */
class DynamicBitWidthCompress(npixels:Int = 16, pixelsize:Int = 10) extends Module {
    val io = IO(new Bundle {
        val in  = Input(Vec(npixels, UInt(pixelsize.W)))
        val out = Output(new DynamicData(maxsize = pixelsize, elemsize = npixels))
    })

    val shuffle = Module(new BitShuffle(npixels, pixelsize))

    // Shuffle the bits and output
    shuffle.io.in := io.in
    io.out.data := shuffle.io.out

    val fdl = Module(new FindDataLength(nelems = pixelsize, elemsize = npixels))
    fdl.io.in := shuffle.io.out
    io.out.len := fdl.io.len

    override def desiredName = "DBWCompress"
}

object DynamicBitWidthCompress extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new DynamicBitWidthCompress))
    )
}
