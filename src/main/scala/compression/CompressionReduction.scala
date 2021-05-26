package compression

import chisel3._
import chisel3.util._

/** Compression.
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReduction(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val maxblocks:Int = 128) extends Module {

    val big_one: BigInt = 1

    val io = IO(new Bundle {
        // The raw 10-bit pixel data from a single shift (128x8 pixels).
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(10.W))))

        // Tell the module if there is still room to write. If high and data is supposed to be written, it will be dropped. 
        // Reaction is one tick delayed -> should be set high when there is only room for one write left (or earlier).
        val fifo_full = Input(Bool())

        // When high, the raw input data will be written to the fifo.
        val bypass_compression = Input(Bool())

        // Whether to use poisson encoding
        val poisson = Input(Bool())

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
        val blocks = Output(Vec(11, UInt(1024.W)))

        // Number of blocks used (max = 10).
        val blocks_used = Output(UInt(4.W))

        // Whether the data in the blocks output should be written to the FIFO.
        // This should never be high when the FIFO is full because of the fifo_full input.
        val write_enable = Output(Bool())

        // Whether data was dropped because of a fifo_full
        val data_dropped = Output(Bool())
    })

    // Initialize frame counter
    val frame_counter = Module(new FrameCounter(16))
    frame_counter.io.frame_sync := io.frame_sync
    frame_counter.io.data_valid := io.data_valid
    frame_counter.io.soft_rst := io.soft_rst

    val shift_num = Wire(UInt(16.W))
    val received_first_sync = Wire(Bool())
    shift_num := frame_counter.io.shift_num
    received_first_sync := frame_counter.io.received_first_sync

    val compressor = Module(new CompressionReductionNoPacking(pixel_rows, pixel_cols, maxblocks))
    compressor.io.pixels := io.pixels
    compressor.io.poisson := io.poisson

    // Group the output into 64-bit blocks
    val padded_compressor_out = compressor.io.out :+ ((1 << 16) - 1).U :+ ((1 << 16) - 1).U :+ ((1 << 16) - 1).U
    val compressed_64 = (0 until compressor.io.out.size by 4).map(i => Cat(padded_compressor_out.slice(i, i+4)))

    // Pass the data through our merger / 2 block ensurer
    val packer = Module(new ShiftPacker(pixel_rows/2*(10*16 + 6), 64, 8))
    packer.io.soft_rst := io.soft_rst
    packer.io.poisson := io.poisson
    packer.io.in := compressed_64
    // Only send if the data is valid and we have received the first sync pulse
    when (io.data_valid && (received_first_sync || io.frame_sync) && ~io.soft_rst) {
        packer.io.len := (compressor.io.outlen +& 3.U) / 4.U
    }.otherwise {
        packer.io.len := 0.U
    }
    packer.io.frame_num := shift_num
    packer.io.fifo_full := io.fifo_full

    // Add a pipeline stage at the end
    val data_reg = List.fill(11)(RegInit(((big_one << 1024)-1).U(1024.W)))
    val blocks_used_reg = RegInit(0.U(4.W))
    val write_enable_reg = RegInit(0.B)
    val data_dropped_reg = RegInit(0.B)

    // Implement the bypass feature
    when (io.bypass_compression) {
        for (i <- 0 until 10) {
            data_reg(i) := Cat((0 until pixel_rows).map(j => Cat(io.pixels(j))))((10-i)*1024-1, (9-i)*1024)
        }
        data_reg(10) := 0.U
        blocks_used_reg := 10.U
        write_enable_reg := ~io.fifo_full && ~io.soft_rst
        data_dropped_reg := io.fifo_full
    }.otherwise {
        for (i <- 0 until 11) {
            data_reg(i) := packer.io.out(i)
        }
        blocks_used_reg := packer.io.blocks_used
        write_enable_reg := packer.io.write_enable
        data_dropped_reg := packer.io.data_dropped
    }

    for (i <- 0 until 11) {
        io.blocks(i) := data_reg(i)
    }
    io.blocks_used := blocks_used_reg
    io.write_enable := write_enable_reg
    io.data_dropped := data_dropped_reg
}

object CompressionReduction extends App {
    chisel3.Driver.execute(args, () => new CompressionReduction)
}
