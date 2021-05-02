package compression

import chisel3._
import chisel3.util._

/** Compression.
 *
 *  @author Sebastian Strempfer
 *  @todo Add sync pulse input and set last 4 bits of frame counter to zero when high - DONE
 *  @todo soft reset is on for at least several ticks. Reset counters and flush data (don't write)
 *  @todo set data valid low until first sync pulse - DONE
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
        // The raw 10-bit pixel data from a single shift (128x8 pixels).
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(10.W))))

        // Tell the module if there is still room to write. If high and data is supposed to be written, it will be dropped. 
        // Reaction is one tick delayed -> should be set high when there is only room for one write left (or earlier).
        val fifo_full = Input(Bool())

        // When high, the raw input data will be written to the fifo.
        val bypass_compression = Input(Bool())

        // Frame sync should be set high every 16 ticks on the first shift of a new frame. 
        // It resets the last 4 bits of the shift number to 0 and increases the frame number.
        val frame_sync = Input(Bool())

        // If data valid is low, the data will not be used and not be counted as a shift (the shift number won't increase).
        // This can be used to artificially lower the framerate by selectively skipping shifts.
        val data_valid = Input(Bool())

        // Soft reset will reset the shift number to zero and make the module wait until the next frame sync before sending data again.
        // It will also stop the merge module (EnsureBlocks) from accepting new data and flush out what it currently has.
        // It should take 2 ticks for the module to completely reset.
        val soft_rst = Input(Bool())

        // When compressing, this will be the compressed data from multiple shifts merged together, formatted into 1024-bit words to fit the FIFO.
        // Otherwise it will be the raw pixel data row by row.
        val blocks = Output(Vec(10, UInt(1024.W)))

        // Number of blocks used (max = 10).
        val blocks_used = Output(UInt(4.W))

        // Whether the data in the blocks output should be written to the FIFO.
        // This should never be high when the FIFO is full because of the fifo_full input.
        val write_enable = Output(Bool())

        // Whether data was dropped because of a fifo_full
        val data_dropped = Output(Bool())
    })

    // Register to store whether the first sync pulse was received and we should start sending data
    val received_first_sync = RegInit(0.B)
    when (io.soft_rst) {
        received_first_sync := 0.B
    }
    when (io.frame_sync && ~io.soft_rst) {
        received_first_sync := 1.B
    }

    // Register which increments every tick
    val shift_num = RegInit(0.U(16.W))
    when (io.soft_rst) {
        shift_num := 0.U
    }.otherwise {
        when (io.frame_sync) {
            // On frame sync, extract the frame number (discarding the last 4 shift number bits), increase it by one, and set the shift number to zero.
            // This way if the sync pulse and shift_num get out of sync, we start over on a new frame number. If they are in sync, the frame number would've been advanced that tick anyways.
            shift_num := Cat(shift_num(15, 4) + 1.U, 0.U(4.W))
        }.otherwise {
            when (io.data_valid && received_first_sync) {
                shift_num := shift_num + 1.U
            }.otherwise {
                shift_num := shift_num
            }
        }
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
    block_merger.io.soft_rst := io.soft_rst
    block_merger.io.in := reduced_64
    // Only send if the data is valid and we have received the first sync pulse
    when (io.data_valid && received_first_sync && ~io.soft_rst) {
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
        write_enable_reg := ~io.fifo_full && ~io.soft_rst
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
