package compression

import chisel3._
import chisel3.util._

/** Inserts metadata at the end of every block where there is room
 *
 *  @author Sebastian Strempfer
 *
 *  @param inbits How many bits the ensureblocks stage has as input
 *  @param wordsize The wordsize of the elements in the input to the ensureblocks stage
 *  @param reservedbits How many bits in the beginning are reserved
 *  @param len width of the metadata to be inserted
 */
class InsertEndMetadata(val inbits: Int = 1024*10, val wordsize: Int = 64, val reservedbits:Int = 8, val metadatawidth:Int = 16) extends Module {
    val inwords = (inbits + wordsize-1)/wordsize

    val io = IO(new Bundle {
        val blocks = Input(Vec(10, UInt(1024.W)))       // The blocks
        val len = Input(UInt((log2Floor(inwords)+1).W)) // Number of wordsize-bit blocks in the input
        val metadata = Input(UInt(metadatawidth.W))               // The metadata to insert
        val out = Output(Vec(10, UInt(1024.W)))         // The blocks with metadata
    })

    for (i <- 0 until 10) {
        val maxblocks = (i*(1024 - reservedbits) + 1024 - reservedbits - metadatawidth) / wordsize
        when (io.len <= maxblocks.U) {
            io.out(i) := Cat(io.blocks(i)(1023, metadatawidth), io.metadata)
        }.otherwise {
            io.out(i) := io.blocks(i)
        }
    }
}

object InsertEndMetadata extends App {
    chisel3.Driver.execute(args, () => new InsertEndMetadata)
}
