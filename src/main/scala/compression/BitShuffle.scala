package compression

import chisel3._
import chisel3.util._ 
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Shuffle bits between pixels to group bits by significance into "pixels".
 *
 *  @author Kazutomo Yoshii <kazutomo.yoshii@gmail.com>
 *
 *  @param nelems The number elems per channel
 *  @param elemsize The number of bits per elem
 */
class BitShuffle(val nelems:Int = 16, val elemsize:Int = 10) extends Module {
    require(nelems > 0)
    require(elemsize > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(nelems, UInt(elemsize.W)))
        val out = Output(Vec(elemsize, UInt(nelems.W)))
    })

    for (i <- 0 until elemsize) {
        val tmp = Wire(Vec(nelems, Bits(1.W)))
        for (j <- 0 until nelems) {
            tmp(j) := io.in(j)(i)
        }
        io.out(i) := Reverse(Cat(tmp))
    }
}

object BitShuffle extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new BitShuffle))
    )
}
