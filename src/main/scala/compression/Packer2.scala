package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.experimental.BundleLiterals._

import scala.math.max

/** Module that merges blocks until adding new data would make the length exceed 2048 bits. Also adds metadata to the beginning of every 1024-bit block.
 *  Slightly more advanced version than Packer. Adds support for may_write input
 *  
 *  @author Sebastian Strempfer
 *
 *  @constructor Create a new EnsureBlocks module with custom parameters
 *  @param inwords Maximum number of words the module can receive per tick
 *  @param wordsize Size of the input words of data
 *  @param numblocks Number of 1024-bit blocks to write at once
 *  @param inputs_per_write How many inputs will need to be processed until may_write is true
 */
class Packer2(inwords:Int = (64*10 + 64*6/16) / 4, wordsize:Int = 16, numblocks:Int = 16, inputs_per_write:Int = 4) extends Module {
    val headerbits: Int = 16

    require(inwords > 0)
    require(wordsize > 0)
    require(inputs_per_write > 0)
    require(inwords*wordsize*inputs_per_write <= numblocks*(1024 - headerbits), "Not enough output to fit the worst-case input")
    require(isPow2(wordsize))
    require(numblocks*(1024 - headerbits) % wordsize == 0, "The words do not fit into the blocks evenly")

    // Number of words we can write at once
    val words_per_write = numblocks*(1024 - headerbits)/wordsize
    val reg_size = words_per_write + (inputs_per_write - 1) * inwords
    val reg_blocks = (reg_size * wordsize + 1023) / (1024 - headerbits)

    val io = IO(new Bundle {
        val in = Input(new DynamicData(inwords, elemsize = wordsize))  // Incoming data
        val frame_num = Input(UInt(16.W))                           // Frame number of the data
        val fifo_full = Input(Bool())                               // almost full signal from FIFO (may discard data when high)
        val may_write = Input(Bool())                               // More general than fifo_full, but has the same impact as an inverted fifo_full. Module will only output data when true
        val poisson = Input(Bool())
        val soft_rst = Input(Bool())                                // soft reset will cause the module to "write out the data" immediately while keeping the write enable low
        val out = Output(Vec(numblocks, UInt(1024.W)))              // 11 1024-bit output words
        val blocks_used = Output(UInt((log2Floor(numblocks)+1).W))  // How many blocks contain data (should always be the max)
        val write_enable = Output(Bool())                           // Whether to write the output to the FIFO
        val data_dropped = Output(Bool())                           // Flag that turns on when data is dropped and turns off after the next successful write
    })

    val data_reg = RegInit(0.U.asTypeOf(new DynamicData(reg_size, elemsize = wordsize)))
    val data_positions_reg = RegInit(VecInit(Seq.fill(reg_blocks)(63.U(6.W))))
    val frame_nums_reg = RegInit(VecInit(Seq.fill(reg_blocks)(0.U(8.W))))

    // Merger to pack the shifts together
    val merger = Module(new Merge(
        wordsize = wordsize, 
        granularity = 1, 
        inwords1 = reg_size,
        inwords2 = inwords,
    ))
    merger.io.in1 := data_reg
    merger.io.in2 := io.in

    val should_write = merger.io.out.len >= words_per_write.U
    val can_write = should_write && io.may_write && ~io.fifo_full && ~io.soft_rst

    // Keep track of when data was dropped (set to true when trying to write and fifo full)
    val data_dropped_reg = RegInit(0.B)
    val data_dropped = Wire(Bool())
    data_dropped := merger.io.out.len > reg_size.U && ~can_write
    when (data_dropped) {
        data_dropped_reg := true.B
    }.elsewhen (can_write || io.soft_rst) {
        data_dropped_reg := false.B
    }
    io.data_dropped := data_dropped_reg || data_dropped

    // Calculate where the new input will be located
    val starts_in_block = Wire(UInt(log2Ceil(reg_blocks).W))
    starts_in_block := (data_reg.len * (wordsize / headerbits).U / ((1024 - headerbits) / headerbits).U)
    val pos_in_block = (data_reg.len * (wordsize / headerbits).U) % ((1024 - headerbits) / headerbits).U
    when (io.soft_rst) {
        for (i <- 0 until reg_blocks) {
            data_positions_reg(i) := 63.U
            frame_nums_reg(i) := 0.U
        }
    }.elsewhen (can_write) {
        // Reset all positions except for the first couple
        // where we need to copy positions to or set the position of the current input
        for (i <- 0 until reg_blocks - numblocks) {
            when (starts_in_block === (numblocks + i).U && data_positions_reg(numblocks + i) === 63.U) {
                data_positions_reg(i) := pos_in_block
                frame_nums_reg(i) := io.frame_num
            }.otherwise {
                data_positions_reg(i) := data_positions_reg(numblocks + i)
                frame_nums_reg(i) := frame_nums_reg(numblocks + i)
            }
        }
        for (i <- reg_blocks - numblocks until reg_blocks) { 
            data_positions_reg(i) := 63.U // 63 is an unused value and indicates no shift starting at that position
        }
    }.elsewhen (~data_dropped) {
        // If this is the first shift starting in that block, update the position
        for (i <- 0 until reg_blocks) {
            when (starts_in_block === i.U && data_positions_reg(i) === 63.U) {
                data_positions_reg(i) := pos_in_block
                frame_nums_reg(i) := io.frame_num
            }
        }
    }

    // Create data_positions for the write
    val data_positions = Wire(Vec(numblocks, UInt(6.W)))
    val frame_nums = Wire(Vec(numblocks, UInt(8.W)))
    for (i <- 0 until numblocks) {
        when (starts_in_block === i.U && data_positions_reg(i) === 63.U) {
            data_positions(i) := pos_in_block
            frame_nums(i) := io.frame_num
        }.otherwise {
            data_positions(i) := data_positions_reg(i)
            frame_nums(i) := frame_nums_reg(i)
        }
    }

    when (can_write) {
        // Set register to store all data that doesn't fit in the write
        for (i <- words_per_write until merger.io.out.data.length) data_reg.data(i - words_per_write) := merger.io.out.data(i)
        data_reg.len  := merger.io.out.len - words_per_write.U
    }.elsewhen(~data_dropped) {
        data_reg.data := merger.io.out.data.slice(0, reg_size)

        when (io.soft_rst) {
            data_reg.len := 0.U
        }.otherwise {
            data_reg.len := merger.io.out.len
        }
    }

    // Don't write if the FIFO is full or we are resetting
    io.write_enable := can_write

    // We always fill up all of them
    io.blocks_used := numblocks.U

    val merger_out_uint = Cat(merger.io.out.data.slice(0, words_per_write))
    for (i <- 0 until numblocks) {
        // block without first bit
        val outdata = Cat(io.poisson, data_positions(i), frame_nums(i), merger_out_uint((numblocks - i)*(1024 - headerbits) - 1, (numblocks - i - 1)*(1024 - headerbits)))
        io.out(i) := Cat(!outdata.xorR(), outdata)
    }
}

object Packer2 extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new Packer2))
    )
}
