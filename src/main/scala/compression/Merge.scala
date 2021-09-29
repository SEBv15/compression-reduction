package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.{min, max}

/** Merge module which takes in two vecs of uints and their number of used elements, and outputs a single merged vec and its length. 
 *  Merges data by shifting the second input until there is no gap using ln(inwords1) stages.
 *
 *  @author Kazutomo Yoshii
 *  @author Sebastian Strempfer
 *
 *  @param wordsize UInt width of the vec elements
 *  @param inwords1 Number of elements for the first input vec
 *  @param inwords2 Number of elements for the second input vec
 *  @param minwords1 The minimum number of elements in the first vec
 *  @param maxoutwords The maximum num of output elements (0 = no limit)
 *  @param granularity Round up any input lengths to be a multiple of this number
 */
class Merge(val wordsize:Int = 16, val inwords1:Int = 100, val inwords2:Int = 100, val minwords1:Int = 0, val maxoutwords:Int = 0, val granularity:Int = 1) extends Module {
    require(wordsize > 0)
    require(inwords1 > 0)
    require(inwords2 > 0)
    require(granularity > 0 && isPow2(granularity), "Granularity must be a positive power of two")
    require(inwords1 % granularity == 0, "Max input lengths must be a multiple of the granularity")
    require(minwords1 >= 0)
    require(minwords1 < inwords1)

    val outwords = if (maxoutwords > 0) min(inwords1 + inwords2, maxoutwords) else inwords1 + inwords2

    val gran_log = log2Floor(granularity)

    val io = IO(new Bundle {
        val in1 = Input(new DynamicData(inwords1, elemsize = wordsize))
        val in2 = Input(new DynamicData(inwords2, elemsize = wordsize))
        val out = Output(new DynamicData(outwords, elemsize = wordsize))
    })

    // Number of bits needed to represent how much to the shift the second input
    val shiftsize = log2Ceil(inwords1 - minwords1 + 1)

    // Calculate how much the second input needs to be shifted to the left.
    // The bits in the number also correspond to which shift stages should be enabled.
    val shift = Wire(UInt(shiftsize.W))
    shift := inwords1.U - io.in1.len

    // Generate the shift stages
    val shifters = Seq.tabulate(shiftsize - gran_log)(i => Module(new MergeShifter(wordsize, inwords1 + inwords2, 1 << i + gran_log)).io)

    // Give the first stage the second set of data as input
    for (i <- 0 until inwords1) {
        shifters(0).in(i) := 0.U
    }
    for (i <- 0 until inwords2) {
        shifters(0).in(i + inwords1) := io.in2.data(i)
    }

    // connect the stages
    for (i <- 0 until shifters.length) {
        shifters(i).enable := shift(i + gran_log)
    }
    for (i <- 1 until shifters.length) {
        shifters(i).in := shifters(i - 1).out
    }

    // Assign the values to the output from the correct input
    for (i <- 0 until min(inwords1, outwords)) {
        when (i.U < io.in1.len) {
            io.out.data(i) := io.in1.data(i)
        }.otherwise {
            io.out.data(i) := shifters(shifters.length - 1).out(i)
        }
    }
    for (i <- inwords1 until outwords) {
        io.out.data(i) := shifters(shifters.length - 1).out(i)
    }

    if (granularity > 1) {
        // Make the input lengths be a multiple of the granularity
        val padded1 = Cat((io.in1.len +& (granularity-1).U)(log2Floor(inwords1), gran_log), 0.U(gran_log.W))
        var padded2 = Cat((io.in2.len +& (granularity-1).U)(log2Floor(inwords2), gran_log), 0.U(gran_log.W))
        if (inwords2 % granularity != 0) {
            padded2 = io.in2.len 
        }

        io.out.len := padded1 +& padded2
    } else {
        io.out.len := io.in1.len +& io.in2.len
    }

    override def desiredName = s"Merge_$inwords1"
}

object Merge extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new Merge))
    )
}
