package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Wrapper for CompressionReduction to simplify usage with verilog.
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReductionClockedWrapper(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val maxblocks:Int = 0, val cycles:Int = 4) extends Module {

    val comp = Module(new CompressionReductionClocked(
        pixel_rows = pixel_rows, 
        pixel_cols = pixel_cols, 
        bits_per_pixel = 10,
        maxblocks = maxblocks,
        cycles = cycles
    ))

    val io = IO(new Bundle {
        val pixels = Input(UInt((pixel_rows*pixel_cols*10).W))
        val fifo_full = Input(Bool())
        val poisson = Input(Bool())
        val bypass_compression = Input(Bool())
        val frame_sync = Input(Bool())
        val data_valid = Input(Bool())
        val soft_rst = Input(Bool())
        val sync = Input(Bool())
        val blocks = Output(Vec(comp.numblocks, UInt(1024.W)))
        val blocks_used = Output(UInt((log2Floor(comp.numblocks)+1).W))
        val write_enable = Output(Bool())
        val data_dropped = Output(Bool())
    })

    for (i <- 0 until pixel_rows) {
        for (j <- 0 until pixel_cols) {
            comp.io.pixels(i)(j) := io.pixels(io.pixels.getWidth - 10*(i*pixel_cols+j) - 1, io.pixels.getWidth - 10*(i*pixel_cols+j+1))
        }
    }
    comp.io.fifo_full := io.fifo_full
    comp.io.bypass_compression := io.bypass_compression
    comp.io.frame_sync := io.frame_sync
    comp.io.data_valid := io.data_valid
    comp.io.soft_rst := io.soft_rst
    comp.io.poisson := io.poisson
    comp.io.sync := io.sync
    io.blocks := comp.io.blocks
    io.blocks_used := comp.io.blocks_used
    io.write_enable := comp.io.write_enable
    io.data_dropped := comp.io.data_dropped

    override def desiredName = s"CompressionReductionClockedWrapper_${pixel_rows}_${cycles}"
}

object CompressionReductionClockedWrapper extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new CompressionReductionClockedWrapper))
    )
}
