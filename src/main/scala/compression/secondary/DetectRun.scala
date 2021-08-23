package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Detect whether the input contains a single run of 1s.
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The number of bools in the input Vec
 */
class DetectRun(length: Int) extends Module {
    require(length > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(length, Bool()))
        val is_run = Output(Bool())
    })

    val invalid = Wire(Vec(length, Bool()))
    val run_finished = Wire(Vec(length, Bool()))
    invalid(0) := 0.B
    run_finished(0) := 0.B
    for (i <- 1 until length) {
        run_finished(i) := run_finished(i - 1) || (io.in(i) === 0.B && io.in(i - 1) === 1.B) // Set run finished to true when the input changes from 1 to 0
        invalid(i) := invalid(i - 1) || (run_finished(i - 1) && io.in(i)) // Set invalid (multiple runs) to true if we already finished a run and encounter another 1
    }

    io.is_run := ~invalid(length - 1) && (run_finished(length - 1) || io.in(length - 1)) // It is a run if there was at least one 1 and not more than one run

    override def desiredName = s"DetectRun_${length}"
}

object DetectRun extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new DetectRun(length = 16)))
    )
}