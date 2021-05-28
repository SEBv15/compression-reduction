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
 *  @param inbits Maximum number of bits the module can receive per tick
 *  @param wordsize Size of the input words of data
 *  @param reservebits How many bits in the beginning to use as metadata. One is used to indicate whether it's the first block, the rest is the frame number.
 */
class ShiftPacker(val inbits:Int = 64*10*16 + 64*6, val wordsize:Int = 64, val reservebits:Int = 8) extends Module {
    val inwords = (inbits + wordsize-1) / wordsize
    require(inwords > 0)
    require(inwords*wordsize <= 11*(1024 - reservebits))
    require(isPow2(wordsize))
    require(reservebits >= 1)

    val fifo_size = 11*(1024 - reservebits)/wordsize
    val num_guaranteed = 2*(1024 - reservebits)/wordsize // Number of words needed to fill 2 1024-bit blocks

    // Generate default value of all ones
    var defaultval: BigInt = 1
    defaultval = (defaultval << wordsize) - 1

    val io = IO(new Bundle {
        val in = Input(Vec(inwords, UInt(wordsize.W)))  // Incoming data
        val len = Input(UInt((log2Floor(inwords)+1).W)) // Number of wordsize-bit blocks in the input
        val frame_num = Input(UInt(16.W))               // Frame number of the data
        val fifo_full = Input(Bool())                   // almost full signal from FIFO (may discard data when high)
        val poisson = Input(Bool())
        val soft_rst = Input(Bool())                    // soft reset will cause the module to "write out the data" immediately while keeping the write enable low
        val out = Output(Vec(11, UInt(1024.W)))         // 11 1024-bit output words
        val blocks_used = Output(UInt(4.W))             // How many blocks contain data
        val write_enable = Output(Bool())               // Whether to write the output to the FIFO
        val data_dropped = Output(Bool())               // Flag that turns on when data is dropped and turns off after the next successful write
    })


    // --------- REGISTER DECLARATION --------------

    val hold_reg = List.fill(fifo_size)(RegInit(defaultval.U(wordsize.W))) // Register for the data
    val len_reg = RegInit(0.U((log2Floor(fifo_size)+1).W)) // Register for how many elements are used
    val frame_num_reg = RegInit(0.U(16.W)) // Keep the frame number of the first frame in the register to output as frame number in the metadata
    val data_dropped_reg = RegInit(0.B)
    val blocks_merged_reg = RegInit(0.U(7.W))
    
    // Make the value of the data register a vector for easier use later
    val reg_vec = Wire(Vec(fifo_size, UInt(wordsize.W)))
    for (i <- 0 until fifo_size) {
        reg_vec(i) := hold_reg(i)
    }

    // --------- MERGE MODULE ----------------------

    // Merger to merge the old data with the new
    //val merger = Module(new Merger(wordsize, fifo_size, inwords, 0, 0, false, fifo_size)) // Could possibly be optimized, but this works
    val merger = Module(new MergeStaged(wordsize, fifo_size, inwords, 0, fifo_size)) // Could possibly be optimized, but this works

    // Connect the merger output to the registers
    for (i <- 0 until fifo_size) {
        hold_reg(i) := merger.io.out(i)
    }
    len_reg := merger.io.outlen
    
    // Set the first merger input as the register
    for (i <- 0 until fifo_size) {
        merger.io.data1(i) := hold_reg(i)
    }
    // Set the second input as the new data
    for (i <- 0 until inwords) {
        merger.io.data2(i) := io.in(i)
    }
    merger.io.len2 := io.len // We always input the new data into the merge module

    // --------- MAIN LOGIC -----------------------

    val combinedlen = io.len +& len_reg
    // Decide whether to output the data and clear the register.
    when (combinedlen > fifo_size.U || io.soft_rst) {
        merger.io.len1 := 0.U            // Discard the data that was just written to the FIFO
        io.write_enable := ~io.fifo_full && ~io.soft_rst // Write out the data in the register (or don't if the FIFO say it's full)
        frame_num_reg := io.frame_num    // Set the frame num of the next write to the frame number of the new data that is just incoming
        data_dropped_reg := io.fifo_full
        io.data_dropped := io.fifo_full

        when (io.len === 0.U) {
            blocks_merged_reg := 0.U
        }.otherwise {
            blocks_merged_reg := 1.U
        }
    }.otherwise {
        merger.io.len1 := len_reg
        io.write_enable := 0.B
        when (len_reg === 0.U) {
            frame_num_reg := io.frame_num
        }.otherwise {
            frame_num_reg := frame_num_reg
        }
        data_dropped_reg := data_dropped_reg
        io.data_dropped := data_dropped_reg

        when (merger.io.outlen === 0.U && io.len === 0.U) {
            blocks_merged_reg := 0.U
        }.otherwise {
            when (io.len === 0.U) {
                blocks_merged_reg := blocks_merged_reg
            }.otherwise {
                blocks_merged_reg := blocks_merged_reg +& 1.U
            }
        }
    }

    // --------- FORMAT REGISTER DATA FOR FIFO ----

    val metadata_inserter = Module(new InsertEndMetadata(1024*11, wordsize, reservebits, 16))
    metadata_inserter.io.len := len_reg
    metadata_inserter.io.metadata := Cat(io.poisson, frame_num_reg(14, 0))

    val first_block_metadata = Wire(UInt(8.W))
    first_block_metadata := Cat(1.U(1.W), blocks_merged_reg)
    val default_metadata = Wire(UInt(8.W))
    default_metadata := Cat(0.U(1.W), frame_num_reg(6, 0))

    // Take the data from the register and put it into 10 1024-bit words with metadata at the front.
    for (i <- 0 until 11) {
        val vecmin = fifo_size * wordsize - i*(1024-reservebits) - 1 // Leftmost bit index
        val vecmax = fifo_size * wordsize - (i+1)*(1024-reservebits) // Rightmost bit index
        val paddingbits = 1024 - reservebits - (vecmin + 1 - max(vecmax, 0)) // At the end there will not be enough elements to fill an entire 1024-bit word. We need padding bits there.

        // Generate the padding as all 1s.
        val padding = Wire(UInt(paddingbits.W))
        val numberone: BigInt = 1
        padding := ((numberone << paddingbits) - 1).U

        val metadata = if (i == 0) first_block_metadata else default_metadata

        // Add the metadata in the beginning, set the first bit to 1 if i==0, and add the data after.
        // if (vecmin > 0) {
        //     io.out(i) := Cat(Cat(metadata, Cat(reg_vec)(vecmin, max(vecmax, 0))), padding)
        // } else {
        //     io.out(i) := Cat(metadata, ((numberone << 1024-reservebits)-1).U((1024-reservebits).W))
        // }
        if (vecmin > 0) {
            metadata_inserter.io.blocks(i) := Cat(Cat(metadata, Cat(reg_vec)(vecmin, max(vecmax, 0))), padding)
        } else {
            metadata_inserter.io.blocks(i) := Cat(metadata, ((numberone << 1024-reservebits)-1).U((1024-reservebits).W))
        }
        io.out(i) := metadata_inserter.io.out(i)
    }

    // Calculate the number of blocks used by ceil dividing by elems-per-block. Also, can't have just 1 block. In that case we just send a completely empty block (very rare)
    val tentative_blocks_used = (len_reg*wordsize.U +& (1024-reservebits - 1).U) / (1024-reservebits).U
    when (tentative_blocks_used =/= 1.U) {
        io.blocks_used := tentative_blocks_used
    }.otherwise {
        io.blocks_used := 2.U(4.W)
    }
}

object ShiftPacker extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new ShiftPacker))
    )
}
