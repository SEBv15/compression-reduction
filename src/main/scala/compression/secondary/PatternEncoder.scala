package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Takes in a 16-bit UInt and returns the input encoded using 8 bits if possible. 
 *  The module can encode any input with one or two 1 digits, or any single run of 1 digits. It cannot encode all 0 digits.
 *  
 *  The output is decoded as follows:
 *  Take the sum of out[3:0] + out[6:4] (using inclusive verilog notation), and determine how to proceed based on the result.
 * 
 *  If sum < 15:
 *    out[6:4] encodes the position of the first 1 digit from hi
 *    out[3:0] encodes the position of the first 1 digit from lo
 *    out[7] encodes the value of the bits between the positions
 *  If sum > 15:
 *    Do the same thing but subtract out[6:4] and out[3:0] from 15 before
 *  If sum == 15:
 *    out[7:4] encodes the position of the lone 1 digit starting from lo
 *
 *  Canencode is true when the module could encode the input. Otherwise the output is garbage.
 *
 *  Sample Python Decoding:
 *  def PatternDecode(data):
 *    lo = data & 15
 *    hi = (data >> 4) & 7
 *    if hi + lo == 15:
 *      return 1 << (data >> 4)
 *    if hi + lo > 15:
 *      hi = 15 - hi
 *      lo = 15 - lo
 *    if data >> 7:
 *      return ((1 << (16 - hi - lo)) - 1) << lo
 *    return (1 << (15 - hi)) + (1 << lo)
 *
 *  @author Sebastian Strempfer
 */
class PatternEncoder extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(16.W))
        val out = Output(UInt(8.W))
        val canencode = Output(Bool())
    })

    // Check if we can report the location of the 1s
    val count_ones = Module(new CountOnes(16)).io
    count_ones.in := io.in.asBools()

    // Check if we can report that there is a run of 1s between the encoded endpoints
    val detect_run = Module(new DetectRun(16)).io
    detect_run.in := io.in.asBools()

    val first_one_from_lo = Module(new FirstOne(16)).io
    first_one_from_lo.in := io.in.asBools()

    val first_one_from_hi = Module(new FirstOne(16)).io
    first_one_from_hi.in := io.in.asBools().reverse

    io.canencode := count_ones.out === 1.U || count_ones.out === 2.U || detect_run.is_run

    when (count_ones.out === 1.U) {
        io.out := Cat(first_one_from_lo.out(3, 0), 15.U(4.W) - first_one_from_lo.out(2, 0))
    }.elsewhen (first_one_from_hi.out(3)) {
        io.out := Cat(detect_run.is_run, 7.U(3.W) - first_one_from_hi.out(2,0), 15.U(4.W) - first_one_from_lo.out(3, 0))
    }.otherwise {
        io.out := Cat(detect_run.is_run, first_one_from_hi.out(2,0), first_one_from_lo.out(3, 0))
    }

    override def desiredName = "PatternEncoder"
}

object PatternEncoder extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new PatternEncoder))
    )
}
