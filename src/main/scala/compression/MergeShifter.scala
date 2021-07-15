package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.min
import scala.math.max

/** Takes in a vector and shifts its elements to the left by [shift] if enable is true. Otherwise does nothing.
 *
 *  @author Kazutomo Yoshii
 *  @author Sebastian Strempfer
 *
 *  @param wordsize UInt width of the vec elements
 *  @param length The length of the vector
 *  @param shift How much the elements should be shifted to the left if enable is true
 *  @param ignore How many elements at the start of the vector to ignore (simply pass through)
 */
class MergeShifter(wordsize:Int = 16, length:Int = 10, shift:Int = 1, var ignore:Int = 0) extends Module {
    require(wordsize > 0)
    require(shift < length)
    require(shift > 0)
    if (ignore < 0) ignore = 0

    val io = IO(new Bundle {
        val enable = Input(Bool())
        val in = Input(Vec(length, UInt(wordsize.W)))
        val out = Output(Vec(length, UInt(wordsize.W)))
    })

    for (i <- 0 until ignore) {
        io.out(i) := io.in(i)
    }
    for (i <- ignore until (length - shift)) {
        when (io.enable) {
            io.out(i) := io.in(i + shift)
        }.otherwise {
            io.out(i) := io.in(i)
        }
    }
    for (i <- (length - shift) until length) {
        io.out(i) := io.in(i)
    }

    override def desiredName = s"MergeShifter_${length}_by_${shift}"
}

object MergeShifter extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new MergeShifter))
    )
}
