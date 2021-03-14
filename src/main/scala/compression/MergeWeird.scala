package compression

import chisel3._
import chisel3.util._

import scala.math.min
import scala.math.max

/** Merge module which takes in two vecs of uints and their number of used elements, and outputs a single merged vec and its length.
 *  Doesn't conserve the order of elements. Moves elements from the end of the second input vec to the gap between the first and second vec.
 *
 *  @author Sebastian Strempfer
 *
 *  @constructor Create a new merge module with custom parameters
 *  @param wordsize UInt width of the vec elements
 *  @param inwords1 Number of elements for the first input vec
 *  @param inwords2 Number of elements for the second input vec
 *  @param minwords1 Number of elements that are guaranteed to be used in the first input vec (min length)
 *  @param minwords2 Number of elements that are guaranteed to be used in the second input vec (min length)
 */
class MergeWeird(val wordsize:Int = 16, val inwords1:Int = 10, val inwords2:Int = 10, val minwords1: Int = 0, val minwords2: Int = 0) extends Module {
    require(wordsize > 0)
    require(inwords1 > 0)
    require(inwords2 > 0)
    require(minwords1 >= 0)
    require(minwords2 >= 0)

    val io = IO(new Bundle {
        val len1 = Input(UInt((log2Floor(inwords1) + 1).W))
        val data1 = Input(Vec(inwords1, UInt(wordsize.W)))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val data2 = Input(Vec(inwords2, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(inwords1 + inwords2) + 1).W))
        val out = Output(Vec(inwords1 + inwords2, UInt(wordsize.W)))
    })

    def createMuxLookupList(position: Int) = {
        val lookups = List.range(max(0, minwords2 - inwords1), min(inwords2, max(inwords2 - inwords1, 0) + position + 1)).map { from2 => from2.U -> io.data2(from2.U) }
        lookups
    }

    val pivot = Wire(UInt((log2Floor(inwords1 + inwords2) + 1).W))
    val len = io.len1 +& io.len2
    when (len > inwords1.U) {
        pivot := len - inwords1.U
    }.otherwise {
        pivot := 0.U
    }

    val offset = pivot - io.len1

    for (i <- 0 until minwords1) {
        io.out(i) := io.data1(i)
    }
    for (i <- minwords1 until inwords1) {
        when (i.U >= io.len1) {
            io.out(i) := MuxLookup(offset + i.U, if (i < inwords1) io.data1(i) else io.data2(i-inwords1), createMuxLookupList(i));
        }.otherwise {
            io.out(i) := io.data1(i)
        }
    }
    for (i <- 0 until inwords2) {
        io.out(inwords1 + i) := io.data2(i)
    }

    io.outlen := io.len1 +& io.len2
}

object MergeWeird extends App {
    chisel3.Driver.execute(args, () => new MergeWeird)
}
