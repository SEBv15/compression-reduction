package compression

import chisel3._
import chisel3.util._

/** Purely combinational compression and reduction circuit that takes in the pixels, compresses them, and puts them into one continuous vector (with optional poisson encoding)
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReductionNoPacking(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val maxblocks:Int = 128) extends Module {
    require(isPow2(pixel_rows))
    require(isPow2(pixel_cols))
    require(pixel_cols == 8)
    require(pixel_rows >= 4)

    val big_one: BigInt = 1
    val outwords = (64*6+128*8*10)/16

    val io = IO(new Bundle {
        // The raw 10-bit pixel data from a single shift (128x8 pixels).
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(10.W))))

        // Whether to use poisson encoding
        val poisson = Input(Bool())

        // When compressing, this will be the compressed data from multiple shifts merged together, formatted into 1024-bit words to fit the FIFO.
        // Otherwise it will be the raw pixel data row by row.
        val out = Output(Vec(outwords, UInt(16.W)))

        // Number of blocks used (max = 10).
        val outlen = Output(UInt((log2Floor(outwords)+1).W))
    })

    // Encode the pixels from 10 bits to 7 using poisson noise
    val encoder = List.fill(pixel_rows)(List.fill(pixel_cols)(Module(new PoissonEncoding)))
    val encoded_pixels = Wire(Vec(pixel_rows, Vec(pixel_cols, UInt(7.W))))
    for (i <- 0 until pixel_rows) {
        for (j <- 0 until pixel_cols) {
            encoder(i)(j).io.in := io.pixels(i)(j)
            encoded_pixels(i)(j) := encoder(i)(j).io.out
        }
    }

    // Compress the pixels in 4x4 squares, choosing the encoded or raw pixels
    val compressors = List.fill(pixel_rows/2)(Module(new LengthCompress(16, 10)))
    when (io.poisson) {
        for (i <- 0 until pixel_rows by 4) {
            for (j <- 0 until pixel_cols by 4) {
                compressors(i/2 + j/4).io.in := (0 until 4).map(x => encoded_pixels(i+x).slice(j, j+4)).reduce((a, b) => a ++ b)
            }
        }
    }.otherwise {
        for (i <- 0 until pixel_rows by 4) {
            for (j <- 0 until pixel_cols by 4) {
                compressors(i/2 + j/4).io.in := (0 until 4).map(x => io.pixels(i+x).slice(j, j+4)).reduce((a, b) => a ++ b)
            }
        }
    }

    // Pass the compressed pixels through the reduction stage
    val reducer = Module(new HierarchicalReduction(pixel_rows/2, 10, 16, maxblocks))
    for (i <- 0 until pixel_rows/2) {
        reducer.io.datain(i) := compressors(i).io.data
        reducer.io.headerin(i) := compressors(i).io.header
    }

    io.out := reducer.io.out
    io.outlen := reducer.io.outlen
}

object CompressionReductionNoPacking extends App {
    chisel3.Driver.execute(args, () => new CompressionReductionNoPacking)
}
