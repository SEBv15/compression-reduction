package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.max

/** Module that merges blocks until adding new data would make the length exceed 2048 bits. Also adds metadata to the beginning of every 1024-bit block.
 *  
 *  @author Sebastian Strempfer
 *
 *  @constructor Create a new EnsureBlocks module with custom parameters
 *  @param inwords Maximum number of words the module can receive per tick
 *  @param wordsize Size of the input words of data
 *  @param granularity Round the number of words to be a multiple of this number (to decrease merger size)
 *  @param numblocks Number of 1024-bit blocks to write at once
 */
class ContinuousPacker(val inwords:Int = 64*10 + 64*6/16, val wordsize:Int = 16, val granularity:Int = 4, val numblocks:Int = 16) extends Module {
    val headerbits: Int = 8

    require(inwords > 0)
    require(granularity > 0)
    require(inwords*wordsize <= numblocks*(1024 - headerbits))
    require(isPow2(wordsize))
    require(numblocks*(1024 - headerbits) % (wordsize * granularity) == 0, "The words do not fit into the blocks evenly")

    // Number of words we can write at once
    val reg_size = numblocks*(1024 - headerbits)/wordsize

    val io = IO(new Bundle {
        val in = Input(Vec(inwords, UInt(wordsize.W)))              // Incoming data
        val len = Input(UInt((log2Floor(inwords)+1).W))             // Number of wordsize-bit blocks in the input. len=0 inputs will not be counted in blocks_merged. 
        val frame_num = Input(UInt(16.W))                           // Frame number of the data
        val fifo_full = Input(Bool())                               // almost full signal from FIFO (may discard data when high)
        val poisson = Input(Bool())
        val soft_rst = Input(Bool())                                // soft reset will cause the module to "write out the data" immediately while keeping the write enable low
        val out = Output(Vec(numblocks, UInt(1024.W)))              // 11 1024-bit output words
        val blocks_used = Output(UInt((log2Floor(numblocks)+1).W))  // How many blocks contain data (should always be the max)
        val write_enable = Output(Bool())                           // Whether to write the output to the FIFO
        val data_dropped = Output(Bool())                           // Flag that turns on when data is dropped and turns off after the next successful write
    })

    val data_reg = Reg(Vec(reg_size, UInt(wordsize.W)))
    val datalen_reg = RegInit(0.U((log2Floor(reg_size)+1).W))
    val data_positions_reg = RegInit(VecInit(Seq.fill(numblocks)(127.U(7.W))))

    // Merger to pack the shifts together
    val merger = Module(new MergeStaged(
        wordsize = wordsize, 
        granularity = granularity, 
        inwords1 = reg_size,
        inwords2 = inwords,
    ))

    // Since there will sometimes be gaps because of the granularity, fill those with the frame number
    val metadata_padder = Module(new MetadataPadding(
        wordsize = wordsize, 
        length = inwords,
        granularity = granularity,
    ))
    metadata_padder.io.len := io.len
    metadata_padder.io.data := io.in
    metadata_padder.io.metadata := Cat(io.poisson, io.frame_num(14, 0))

    val next_block = merger.io.outlen > reg_size.U

    // Calculate where the new input will be located
    val starts_in_block = Wire(UInt(log2Ceil(numblocks).W))
    starts_in_block := (datalen_reg * (wordsize / headerbits).U / ((1024 - headerbits) / headerbits).U) % numblocks.U
    val pos_in_block = (datalen_reg * (wordsize / headerbits).U) % ((1024 - headerbits) / headerbits).U
    when (io.soft_rst) {
        for (i <- 0 until numblocks) {
            data_positions_reg(i) := 127.U
        }
    }.elsewhen (next_block) {
        // Reset all positions
        // It is possible that the register was previously full and the overflow data is the whole input
        when (starts_in_block === 0.U) {
            data_positions_reg(0) := 0.U
        }.otherwise {
            data_positions_reg(0) := 127.U // 127 is an unused value and indicates no shift starting at that position
        }
        for (i <- 1 until numblocks) {
            data_positions_reg(i) := 127.U // 127 is an unused value and indicates no shift starting at that position
        }
    }.otherwise {
        // If this is the first shift starting in that block, update the position
        when (data_positions_reg(starts_in_block) === 127.U) {
            data_positions_reg(starts_in_block) := pos_in_block
        }
    }
    // Create data_positions that include the shift that triggers the write
    val data_positions = Wire(Vec(numblocks, UInt(7.W)))
    for (i <- 0 until numblocks) {
        // Update the position, but not if starts_in_block is zero. 
        // The zero case can never happen on a write because the data input is less than the register size. 
        // It can however have that value if the register was perfectly filled on the previous iteration.
        when (starts_in_block === i.U && data_positions_reg(i) === 127.U && (i != 0).B) {
            data_positions(i) := pos_in_block
        }.otherwise {
            data_positions(i) := data_positions_reg(i)
        }
    }

    // connect data paths to registers
    merger.io.len1 := datalen_reg
    merger.io.data1 := data_reg
    merger.io.len2 := metadata_padder.io.outlen
    merger.io.data2 := metadata_padder.io.out
    when (next_block && !io.soft_rst) {
        // Set register to store all data that doesn't fit in the write
        for (i <- 0 until merger.io.out.length - reg_size) {
            data_reg(i) := merger.io.out(reg_size + i)
        }
        datalen_reg := merger.io.outlen - reg_size.U
    }.otherwise {
        data_reg := merger.io.out.slice(0, reg_size)

        when (io.soft_rst) {
            datalen_reg := 0.U
        }.otherwise {
            datalen_reg := merger.io.outlen
        }
    }

    // Keep track of when data was dropped (set to true when trying to write and fifo full)
    val data_dropped_reg = RegInit(0.B)
    when (next_block && io.fifo_full && !io.soft_rst) {
        data_dropped_reg := true.B
    }.elsewhen ((next_block && !io.fifo_full) || io.soft_rst) {
        data_dropped_reg := false.B
    }
    io.data_dropped := data_dropped_reg

    // Don't write if the FIFO is full or we are resetting
    io.write_enable := next_block && !io.soft_rst && !io.fifo_full

    // We always fill up all of them
    io.blocks_used := 16.U

    val merger_out_uint = Cat(merger.io.out.slice(0, reg_size))
    for (i <- 0 until numblocks) {
        // block without first bit
        val outdata = Cat(data_positions(i), merger_out_uint((numblocks - i)*(1024 - headerbits) - 1, (numblocks - i - 1)*(1024 - headerbits)))
        io.out(i) := Cat(outdata.xorR(), outdata)
    }
}

object ContinuousPacker extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new ContinuousPacker))
    )
}
