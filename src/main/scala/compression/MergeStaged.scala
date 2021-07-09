package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.min
import scala.math.max

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
class MergeStaged(val wordsize:Int = 16, val inwords1:Int = 100, val inwords2:Int = 100, val minwords1:Int = 0, val maxoutwords:Int = 0, val granularity:Int = 1) extends Module {
    require(wordsize > 0)
    require(inwords1 > 0)
    require(inwords2 > 0)
    require(granularity > 0 && isPow2(granularity), "Granularity must be a positive power of two")
    require(inwords1 % granularity == 0 && inwords2 % granularity == 0, "Max input lengths must be a multiple of the granularity")
    require(minwords1 >= 0)
    require(minwords1 < inwords1)

    val outwords = if (maxoutwords > 0) min(inwords1 + inwords2, maxoutwords) else inwords1 + inwords2

    val gran_log = log2Floor(granularity)

    val io = IO(new Bundle {
        val len1 = Input(UInt((log2Floor(inwords1) + 1).W))
        val data1 = Input(Vec(inwords1, UInt(wordsize.W)))
        val len2 = Input(UInt((log2Floor(inwords2) + 1).W))
        val data2 = Input(Vec(inwords2, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(outwords) + 1).W))
        val out = Output(Vec(outwords, UInt(wordsize.W)))
    })

    // Number of bits needed to represent how much to the shift the second input
    val shiftsize = log2Floor(inwords1 - minwords1) + 1

    // Calculate how much the second input needs to be shifted to the left.
    // The bits in the number also correspond to which shift stages should be enabled.
    val shift = Wire(UInt(shiftsize.W))
    shift := inwords1.U - io.len1

    val stages = Wire(Vec(shiftsize + 1 - gran_log, Vec(inwords1 + inwords2, UInt(wordsize.W))))

    // Set the first stage to be the input for the second vector
    for (i <- 0 until inwords1) {
        stages(0)(i) := 0.U
    }
    for (i <- 0 until inwords2) {
        stages(0)(i+inwords1) := io.data2(i)
    }

    // Go through the stages where each stage can shift the second input by l to the left.
    // ignore indicates how many elements at the start can't possibly have a value for each stage.
    // This is probably already optimized by chisel since it doesn't affect gate count.
    var l = granularity
    for (i <- 0 until shiftsize - gran_log) {
        val s = Module(new MergeStage(wordsize, inwords1 + inwords2, l))
        s.io.in := stages(i)
        stages(i+1) := s.io.out
        s.io.enable := shift(i + gran_log)

        l *= 2
    }

    // Assign the values to the output from the correct input
    for (i <- 0 until min(inwords1, outwords)) {
        when (i.U < io.len1) {
            io.out(i) := io.data1(i)
        }.otherwise {
            io.out(i) := stages(stages.length - 1)(i)
        }
    }
    for (i <- inwords1 until outwords) {
        io.out(i) := stages(stages.length - 1)(i)
    }

    if (granularity > 1) {
        // Make the input lengths be a multiple of the granularity
        val padded1 = Cat((io.len1 +& (granularity-1).U)(log2Floor(inwords1), gran_log), 0.U(gran_log.W))
        val padded2 = Cat((io.len2 +& (granularity-1).U)(log2Floor(inwords2), gran_log), 0.U(gran_log.W))

        io.outlen := padded1 +& padded2
    } else {
        io.outlen := io.len1 +& io.len2
    }

    override def desiredName = s"MergeStaged_$inwords1"
}

object MergeStaged extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new MergeStaged))
    )
}
