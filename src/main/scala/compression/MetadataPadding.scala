package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import scala.math.min
import scala.math.max

/** Makes a variable length Vec be a multiple of a specific value long, and uses custom metadata as padding
 *
 *  @author Sebastian Strempfer
 *
 *  @param wordsize UInt width of the vec elements
 *  @param length Max length of the input data
 *  @param granularity Target granularity
 */
class MetadataPadding(val wordsize:Int = 16, val length:Int = 100, val granularity:Int = 2) extends Module {
    require(wordsize > 0)
    require(length > 0)
    require(granularity > 1 && isPow2(granularity))

    val io = IO(new Bundle {
        val len = Input(UInt((log2Floor(length) + 1).W))
        val data = Input(Vec(length, UInt(wordsize.W)))
        val metadata = Input(UInt(wordsize.W))
        val outlen = Output(UInt((log2Floor(length) + 1).W))
        val out = Output(Vec(length, UInt(wordsize.W)))
    })

    for (i <- 0 until length) {
        when (i.U < io.len) {
            io.out(i) := io.data(i)
        }.otherwise {
            io.out(i) := io.metadata
        }
    }

    io.outlen := Cat((io.len +& (granularity-1).U)(log2Floor(length), log2Floor(granularity)), 0.U(log2Floor(granularity).W))
}

object MetadataPadding extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new MetadataPadding))
    )
}
