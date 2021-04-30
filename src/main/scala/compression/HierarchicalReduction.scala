package compression

import chisel3._
import chisel3.util._

/** Takes in compressor output, reduces the data, and calculates the hierarchical headers. The output is the final data (before adding metadata).
 *  The output will look like this
 *  | 2-bit headers | variable sized 3-bit headers | variable sized data |
 *
 *  @author Sebastian Strempfer
 *
 *  @param ncompressors The number of compressors the module should take data from
 *  @param nwords Number of elements / words each compressor has
 *  @param wordsize Width of the elements / words the compressors give
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class HierarchicalReduction(val ncompressors:Int = 64, val nwords:Int = 7, val wordsize:Int = 16, val maxblocks:Int = 128) extends Module {
    require(isPow2(ncompressors))
    require(ncompressors > 0)
    require(nwords > 0)
    require(wordsize > 0)
    require(isPow2(wordsize))
    require(wordsize <= 128)

    val big_one: BigInt = 1
    val headerwidth = log2Floor(nwords) + 1
    val outsize = ncompressors*nwords + (ncompressors*2 + ncompressors*3 + wordsize-1)/wordsize

    val io = IO(new Bundle {
        val datain = Input(Vec(ncompressors, Vec(nwords, UInt(wordsize.W))))
        val headerin = Input(Vec(ncompressors, UInt(headerwidth.W)))
        val out = Output(Vec(outsize, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(outsize) + 1).W))
    })

    // Make 2-bit headers. Reserve value 3 as undefined
    val twobit_headers = Wire(Vec(ncompressors, UInt(2.W)))
    for (i <- 0 until ncompressors) {
        when (io.headerin(i) >= 2.U) {
            twobit_headers(i) := 2.U
        }.otherwise {
            twobit_headers(i) := io.headerin(i)
        }
    }
    val twobits_per_word = wordsize/2
    val twobit_headers_wordsize = (0 until ncompressors*2/wordsize).map(x => Cat(twobit_headers.slice(twobits_per_word*x, twobits_per_word*(x+1))))

    // Reduce full sized headers together
    val header_reduction = Module(new Reduction(ncompressors, 1, headerwidth, 0, false))
    for (i <- 0 until ncompressors) {
        header_reduction.io.in(i)(0) := io.headerin(i)
        header_reduction.io.inlengths(i) := io.headerin(i) >= 2.U
    }
    val header_uint = Cat(Cat(header_reduction.io.out), ((big_one << wordsize) - 1).U(wordsize.W))
    val headers_wordsize = Wire(Vec((ncompressors*headerwidth + wordsize-1)/wordsize, UInt(wordsize.W)))
    for (i <- 0 until headers_wordsize.size) {
        headers_wordsize(i) := header_uint(header_uint.getWidth - i*wordsize-1, header_uint.getWidth - (i+1)*wordsize)
    }

    // Reduce data
    val data_reduction = Module(new Reduction(ncompressors, nwords, wordsize, 128, true))
    data_reduction.io.in := io.datain
    data_reduction.io.inlengths := io.headerin

    // Merge everything together
    val header_data_merger = Module(new Merger(wordsize, headers_wordsize.size, ncompressors*nwords, 0, 0, true))
    header_data_merger.io.data1 := headers_wordsize
    header_data_merger.io.len1 := (header_reduction.io.outlength*headerwidth.U +& (wordsize-1).U) / wordsize.U
    header_data_merger.io.data2 := data_reduction.io.out
    header_data_merger.io.len2 := data_reduction.io.outlength

    io.out := twobit_headers_wordsize ++ header_data_merger.io.out
    io.outlen := (ncompressors*2/wordsize).U +& header_data_merger.io.outlen
}

object HierarchicalReduction extends App {
    chisel3.Driver.execute(args, () => new HierarchicalReduction)
}
