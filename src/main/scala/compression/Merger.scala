package compression

import chisel3._
import chisel3.util._

import scala.math.min

/** Wrapper for Merge and MergeWeird. Allows swapping merge method quickly using a parameter.
 *
 *  @author Sebastian Strempfer
 *
 *  @param wordsize UInt width of the vec elements
 *  @param inwords1 Number of elements for the first input vec
 *  @param inwords2 Number of elements for the second input vec
 *  @param minwords1 Number of elements that are guaranteed to be used in the first input vec (min length)
 *  @param minwords2 Number of elements that are guaranteed to be used in the second input vec (min length)
 *  @param merge_weird Use the weird (but potentially less gatey) merger
 *  @param maxoutwords The maximum num of output elements (0 = no limit) (only works for merge_weird = false)
 */
class Merger(val wordsize:Int = 16, val inwords1:Int = 10, val inwords2:Int = 10, val minwords1:Int = 0, val minwords2:Int = 0, val merge_weird:Boolean = true, val maxoutwords:Int = 0) extends Module {
    require(!merge_weird || maxoutwords == 0) // maxwords can only be set for the regular merger

    val outwords = if (maxoutwords > 0) min(inwords1 + inwords2, maxoutwords) else inwords1 + inwords2

    val io = IO(new Bundle {
        val len1 = Input(UInt((log2Ceil(inwords1) + 1).W))
        val data1 = Input(Vec(inwords1, UInt(wordsize.W)))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val data2 = Input(Vec(inwords2, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(outwords) + 1).W))
        val out = Output(Vec(outwords, UInt(wordsize.W)))
    })

    if (merge_weird) {
        val weird_merger = Module(new MergeWeird(wordsize, inwords1, inwords2, minwords1, minwords2))
        io <> weird_merger.io
    } else {
        val merger = Module(new Merge(wordsize, inwords1, inwords2, minwords1, maxoutwords))
        io <> merger.io        
    }
}

object Merger extends App {
    chisel3.Driver.execute(args, () => new Merger)
}
