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
 *  @param bits_per_pixel Number of bits per pixel
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class CompressionReductionClocked(val pixel_rows:Int = 128, val pixel_cols:Int = 8, val bits_per_pixel:Int = 10, val maxblocks:Int = 0, val cycles:Int = 8) extends Module {

    val big_one: BigInt = 1
    val reduction_bits: Int = (pixel_rows * pixel_cols / 16 * 6 + pixel_rows * pixel_cols * bits_per_pixel) / cycles
    val numblocks: Int = (cycles * reduction_bits + (1024 - 16 - 1)) / (1024 - 16)

    println("Using " + pixel_rows + " pixel rows")

    val io = IO(new Bundle {
        // The raw 10-bit pixel data from a single shift (128x8 pixels).
        val pixels = Input(Vec(pixel_rows, Vec(pixel_cols, UInt(bits_per_pixel.W))))

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

        // Read and write data on the sync pulse
        val sync = Input(Bool())

        // Soft reset will reset the shift number to zero and make the module wait until the next frame sync before sending data again.
        // It will also stop the merge module (EnsureBlocks) from accepting new data and flush out what it currently has.
        // It should take 2 ticks for the module to completely reset.
        val soft_rst = Input(Bool())

        // When compressing, this will be the compressed data from multiple shifts merged together, formatted into 1024-bit words to fit the FIFO.
        // Otherwise it will be the raw pixel data row by row.
        val blocks = Output(Vec(numblocks, UInt(1024.W)))

        // Number of blocks used (max = 10).
        val blocks_used = Output(UInt((log2Floor(numblocks) + 1).W))

        // Whether the data in the blocks output should be written to the FIFO.
        // This should never be high when the FIFO is full because of the fifo_full input.
        val write_enable = Output(Bool())

        // Whether data was dropped because of a fifo_full
        val data_dropped = Output(Bool())
    })

    // Register all inputs so they are accessible for all cycles that need them
    val pixels_reg = Reg(Vec(pixel_rows, Vec(pixel_cols, UInt(bits_per_pixel.W))))
    val fifo_full_reg = Reg(Bool())
    val data_valid_reg = RegInit(0.B)
    val bypass_compression_reg = RegInit(0.B)
    val poisson_reg = RegInit(0.B)
    val frame_sync_reg = RegInit(0.B)
    val soft_rst_reg = RegInit(1.B)
    when (io.sync) {
        pixels_reg := io.pixels
        fifo_full_reg := io.fifo_full
        data_valid_reg := io.data_valid
        bypass_compression_reg := io.bypass_compression
        poisson_reg := io.poisson
        frame_sync_reg := io.frame_sync
        soft_rst_reg := io.soft_rst
    }

    // Initialize frame counter
    val frame_counter = Module(new FrameCounter(16))
    frame_counter.io.frame_sync := frame_sync_reg
    frame_counter.io.data_valid := data_valid_reg && io.sync
    frame_counter.io.soft_rst := soft_rst_reg

    val shift_num = Wire(UInt(16.W))
    val received_first_sync = Wire(Bool())
    shift_num := frame_counter.io.shift_num
    received_first_sync := frame_counter.io.received_first_sync

    val compressor = Module(new CompressionReductionNoPacking(pixel_rows / cycles, pixel_cols, bits_per_pixel, maxblocks))
    for (i <- 0 until cycles) {
        compressor.io.pixels := pixels_reg.slice(i * pixel_rows / cycles, (i + 1) * pixel_rows / cycles)
    }
    compressor.io.poisson := poisson_reg

    // val soft_rst_pipe = RegNext(soft_rst_reg, 1.B)
    // val poisson_pipe = RegNext(poisson_reg)
    // val out_pipe = RegNext(compressor.io.out)
    // val shift_num_pipe = RegNext(shift_num)
    // val 

    // Pass the data through our merger / 2 block ensurer
    val packer = Module(new Packer2((reduction_bits + 15)/16, 16, numblocks, cycles))
    packer.io.soft_rst := soft_rst_reg
    packer.io.poisson := poisson_reg
    packer.io.may_write := io.sync
    packer.io.in.data := compressor.io.out.data
    // Only send if the data is valid and we have received the first sync pulse
    when (data_valid_reg && (received_first_sync || frame_sync_reg) && ~soft_rst_reg) {
        packer.io.in.len := compressor.io.out.len
    }.otherwise {
        packer.io.in.len := 0.U
    }
    packer.io.frame_num := shift_num
    packer.io.fifo_full := fifo_full_reg

    // Add a pipeline stage at the end
    val data_reg = Reg(Vec(numblocks, UInt(1024.W)))
    val blocks_used_reg = RegInit(0.U((log2Floor(numblocks)+1).W))
    val write_enable_reg = RegInit(0.B)
    val data_dropped_reg = RegInit(0.B)

    // Only modify output registers on sync
    when (io.sync) {
        // Implement the bypass feature
        when (bypass_compression_reg) {
            val rawsize = pixel_rows * pixel_cols * bits_per_pixel
            val rawUIntpadded = Cat(Cat((0 until pixel_rows).map(j => Cat(pixels_reg(j)))), 0.U(1024.W))
            for (i <- 0 until (rawsize + 1023) / 1024) {
                data_reg(i) := rawUIntpadded(rawsize - 1024*(i - 1) - 1, rawsize - 1024*i)
            }
            for (i <- (rawsize + 1023) / 1024 until numblocks) {
                data_reg(i) := 0.U
            }
            blocks_used_reg := ((rawsize + 1023) / 1024).U
            write_enable_reg := ~fifo_full_reg && ~soft_rst_reg
            data_dropped_reg := fifo_full_reg
        }.otherwise {
            for (i <- 0 until numblocks) {
                data_reg(i) := packer.io.out(i)
            }
            blocks_used_reg := packer.io.blocks_used
            write_enable_reg := packer.io.write_enable
            data_dropped_reg := packer.io.data_dropped
        }
    }

    for (i <- 0 until numblocks) {
        io.blocks(i) := data_reg(i)
    }
    io.blocks_used := blocks_used_reg
    io.write_enable := write_enable_reg
    io.data_dropped := data_dropped_reg

    override def desiredName = s"CompressionReductionClocked_${pixel_rows}_${cycles}"
}

object CompressionReductionClocked extends App {
    val rows = scala.util.Properties.envOrElse("ROWS", "128").toInt

    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new CompressionReductionClocked(pixel_rows = rows)))
    )
}
