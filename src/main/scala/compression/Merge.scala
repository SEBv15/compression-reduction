package compression

import chisel3._
import chisel3.util._

import scala.math.min
import scala.math.max

/** Merge module which takes in two vecs of uints and their number of used elements, and outputs a single merged vec and its length. 
 *  Merges data by shifting the second input until there is no gap.
 *
 *  @author Sebastian Strempfer
 *
 *  @param wordsize UInt width of the vec elements
 *  @param inwords1 Number of elements for the first input vec
 *  @param inwords2 Number of elements for the second input vec
 *  @param minwords1 The minimum number of elements in the first vec
 */
class Merge(val wordsize:Int = 16, val inwords1:Int = 10, val inwords2:Int = 10, val minwords1:Int = 0) extends Module {
    require(wordsize > 0)
    require(inwords1 > 0)
    require(inwords2 > 0)
    require(minwords1 >= 0)

    val io = IO(new Bundle {
        val len1 = Input(UInt((log2Floor(inwords1) + 1).W))
        val data1 = Input(Vec(inwords1, UInt(wordsize.W)))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val data2 = Input(Vec(inwords2, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(inwords1 + inwords2) + 1).W))
        val out = Output(Vec(inwords1 + inwords2, UInt(wordsize.W)))
    })

    def createMuxLookupList(position: Int) = {
        // Create cases for when the output element should not be the same as the corresponding input based on len1
        // Here this should only matter when the position is less than len1.
        // Also, when inwords2 is less than inwords1, the second input can be shifted past the element so we don't care either
        val lookups = List.range(max(minwords1, position + 1 - inwords2), min(position + 1, inwords1)).map { len1 => len1.U -> io.data2((position - len1).U) }
        lookups
    }

    for (i <- 0 until inwords1 + inwords2) {
        io.out(i) := MuxLookup(io.len1, if (i < inwords1) io.data1(i) else io.data2(i-inwords1), createMuxLookupList(i));
    }

    io.outlen := io.len1 +& io.len2
}

object Merge extends App {
    chisel3.Driver.execute(args, () => new Merge)
}
