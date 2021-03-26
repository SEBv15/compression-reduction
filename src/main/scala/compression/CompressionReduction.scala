package compression

import chisel3._
import chisel3.util._

/** Compression.
 *
 *  @author Sebastian Strempfer
 *  @todo Add sync pulse input and set last 4 bits of frame counter to zero when high
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReduction(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val maxblocks:Int = 128) extends Module {
    require(isPow2(pixel_rows))
    require(isPow2(pixel_cols))
    require(pixel_cols == 8)
    require(pixel_rows >= 4)

    val big_one: BigInt = 1

    val io = IO(new Bundle {
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(10.W))))
        val fifo_full = Input(Bool())
        val bypass_compression = Input(Bool())
        val frame_sync = Input(Bool())
        val use_nth = Input(UInt(8.W))
        val blocks = Output(Vec(10, UInt(1024.W)))
        val blocks_used = Output(UInt(4.W))
        val write_enable = Output(Bool())
        val data_dropped = Output(Bool())
    })

    // Register which increments every tick
    val shift_num = RegInit(0.U(16.W))
    when (io.frame_sync) {
        // On frame sync, extract the frame number (discarding the last 4 shift number bits), increase it by one, and set the shift number to zero.
        // This way if the sync pulse and shift_num get out of sync, we start over on a new frame number. If they are in sync, the frame number would've been advanced that tick anyways.
        shift_num := Cat(shift_num(15, 4) + 1.U, 0.U(4.W))
    }.otherwise {
        shift_num := shift_num + 1.U
    }

    // Encode the pixels from 10 bits to 7
    val encoder = List.fill(pixel_rows)(List.fill(pixel_cols)(Module(new PoissonEncoding)))
    val encoded_pixels = Wire(Vec(pixel_rows, Vec(pixel_cols, UInt(7.W))))
    for (i <- 0 until pixel_rows) {
        for (j <- 0 until pixel_cols) {
            encoder(i)(j).io.in := io.pixels(i)(j)
            encoded_pixels(i)(j) := encoder(i)(j).io.out
        }
    }

    // Compress the pixels in 4x4 squares
    val compressors = List.fill(pixel_rows/2)(Module(new LengthCompress(16, 7)))
    for (i <- 0 until pixel_rows by 4) {
        for (j <- 0 until pixel_cols by 4) {
            compressors(i/2 + j/4).io.in := (0 until 4).map(x => encoded_pixels(i+x).slice(j, j+4)).reduce((a, b) => a ++ b)
        }
    }

    // Pass the compressed pixels through the reduction stage
    val reducer = Module(new HierarchicalReduction(pixel_rows/2, 7, 16, maxblocks))
    for (i <- 0 until pixel_rows/2) {
        reducer.io.datain(i) := compressors(i).io.data
        reducer.io.headerin(i) := compressors(i).io.header
    }

    // Group the output into 64-bit blocks
    val padded_reducer_out = reducer.io.out :+ ((1 << 16) - 1).U :+ ((1 << 16) - 1).U :+ ((1 << 16) - 1).U
    val reduced_64 = (0 until reducer.io.out.size by 4).map(i => Cat(padded_reducer_out.slice(i, i+4)))

    // Pass the data through our merger / 2 block ensurer
    val block_merger = Module(new EnsureBlocks(pixel_rows/2*(7*16 + 5), 64, 8))
    block_merger.io.in := reduced_64
    // Only use every nth frame
    when (shift_num(15, 4) % io.use_nth === 0.U) {
        block_merger.io.len := (reducer.io.outlen +& 3.U) / 4.U
    }.otherwise {
        block_merger.io.len := 0.U
    }
    block_merger.io.frame_num := shift_num
    block_merger.io.fifo_full := io.fifo_full

    // Add a pipeline stage at the end
    val data_reg = List.fill(10)(RegInit(((big_one << 1024)-1).U(1024.W)))
    val blocks_used_reg = RegInit(0.U(4.W))
    val write_enable_reg = RegInit(0.B)
    val data_dropped_reg = RegInit(0.B)

    // Implement the bypass feature
    when (io.bypass_compression) {
        for (i <- 0 until 10) {
            data_reg(i) := Cat((0 until pixel_rows).map(i => Cat(io.pixels(i))))((10-i)*1024-1, (9-i)*1024)
        }
        blocks_used_reg := 10.U
        write_enable_reg := ~io.fifo_full
        data_dropped_reg := io.fifo_full
    }.otherwise {
        for (i <- 0 until 10) {
            data_reg(i) := block_merger.io.out(i)
        }
        blocks_used_reg := block_merger.io.blocks_used
        write_enable_reg := block_merger.io.write_enable
        data_dropped_reg := block_merger.io.data_dropped
    }

    for (i <- 0 until 10) {
        io.blocks(i) := data_reg(i)
    }
    io.blocks_used := blocks_used_reg
    io.write_enable := write_enable_reg
    io.data_dropped := data_dropped_reg
}

object CompressionReduction extends App {
    chisel3.Driver.execute(args, () => new CompressionReduction)
}
