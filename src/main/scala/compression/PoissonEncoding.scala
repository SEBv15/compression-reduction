package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.pow
import scala.math.max

/** Lossy encode pixel value by adjusting the resolution based on the poisson noise.
 *
 *  @author Mike Hammer, Sebastian Strempfer
 */
class PoissonEncoding extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(10.W))
        val out = Output(UInt(7.W))
    })

    val low = Wire(UInt(4.W))
    low := io.in

    val midlow = Wire(UInt(5.W))
    midlow := (io.in >> 2) +& (-4 + 16).U

    val midhigh = Wire(UInt(6.W))
    midhigh := (io.in >> 3) +& (-8 + 28).U

    val high = Wire(UInt(7.W))
    high := (io.in >> 4) +& (-16 + 52).U

    io.out := Mux(io.in < 16.U, low, Mux(io.in < 64.U, midlow, Mux(io.in < 256.U, midhigh, high)))
}

object PoissonEncoding extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new PoissonEncoding))
    )
}
