package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Takes in a 16-bit UInt and returns the position of the first and last 1 digit encoded in 7 bits.
 *
 *  @author Sebastian Strempfer
 */
class PatternEncoder extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(16.W))
        val out = Output(UInt(7.W))
    })

    // val count_ones = Module(new CountOnes(16)).io
    // count_ones.in := io.in.asBools()
    // io.canencode := count_ones.out === 2.U

    val first_one_from_hi = Module(new FirstOne(16)).io
    first_one_from_hi.in := io.in.asBools()

    val first_one_from_lo = Module(new FirstOne(16)).io
    first_one_from_lo.in := io.in.asBools().reverse

    when (first_one_from_hi.out(3)) {
        io.out := Cat(7.U - first_one_from_hi.out(2,0), 15.U - first_one_from_lo.out)
    }.otherwise {
        io.out := Cat(first_one_from_hi.out(2,0), first_one_from_lo.out)
    }

    override def desiredName = "PatternEncoder"
}

object PatternEncoder extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new PatternEncoder))
    )
}
