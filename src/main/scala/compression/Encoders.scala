package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Compression.
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 */
class Encoders(val pixel_rows:Int = 128, val pixel_cols:Int = 8) extends Module {
    require(isPow2(pixel_rows) && pixel_rows >= 4)
    require(pixel_cols == 8)

    val io = IO(new Bundle {
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(10.W))))
        val poisson = Input(Bool())
        val out = Output(Vec(pixel_rows * pixel_cols / 16, new DynamicData(maxsize = 10, elemsize = 16)))
    })

    // Encode the pixels from 10 bits to 7 using poisson noise
    val encoder = List.fill(pixel_rows)(List.fill(pixel_cols)(Module(new PoissonEncoding)))
    val encoded_pixels = Wire(Vec(pixel_rows, Vec(pixel_cols, UInt(7.W))))
    for {
        i <- 0 until pixel_rows
        j <- 0 until pixel_cols
    } {
        encoder(i)(j).io.in := io.pixels(i)(j)
        encoded_pixels(i)(j) := encoder(i)(j).io.out
    }

    // Compress the pixels in 4x4 squares, choosing the encoded or raw pixels
    val compressors = List.fill(pixel_rows/2)(Module(new DynamicBitWidthCompress(16, 10)))
    when (io.poisson) {
        for {
            i <- 0 until pixel_rows by 4
            j <- 0 until pixel_cols by 4
        } compressors(i/2 + j/4).io.in := (0 until 4).map(x => encoded_pixels(i+x).slice(j, j+4)).reduce((a, b) => a ++ b)
    }.otherwise {
        for {
            i <- 0 until pixel_rows by 4
            j <- 0 until pixel_cols by 4
        } compressors(i/2 + j/4).io.in := (0 until 4).map(x => io.pixels(i+x).slice(j, j+4)).reduce((a, b) => a ++ b)
    }

    for (i <- 0 until compressors.length) {
        io.out(i) := compressors(i).io.out
    }
}

object Encoders extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new Encoders))
    )
}
