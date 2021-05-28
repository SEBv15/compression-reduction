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
class CompressionReductionWrapper(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val maxblocks:Int = 0) extends Module {
    val io = IO(new Bundle {
        val pixels = Input(UInt((pixel_rows*pixel_cols*10).W))
        val fifo_full = Input(Bool())
        val poisson = Input(Bool())
        val bypass_compression = Input(Bool())
        val frame_sync = Input(Bool())
        val data_valid = Input(Bool())
        val soft_rst = Input(Bool())
        val blocks = Output(Vec(11, UInt(1024.W)))
        val blocks_used = Output(UInt(4.W))
        val write_enable = Output(Bool())
        val data_dropped = Output(Bool())
    })

    val comp = Module(new CompressionReduction(pixel_rows, pixel_cols, maxblocks))
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
    io.blocks := comp.io.blocks
    io.blocks_used := comp.io.blocks_used
    io.write_enable := comp.io.write_enable
    io.data_dropped := comp.io.data_dropped
}

object CompressionReductionWrapper extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new CompressionReductionWrapper))
    )
}
