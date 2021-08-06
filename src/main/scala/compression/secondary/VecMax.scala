package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Find the index and value of the maximum element (prefers lower index elements)
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The number of elements in the Vec
 *  @param elemsize The bitwidth of the elements
 */
class VecMax(length: Int, elemsize: Int) extends Module {
    require(length > 0)
    require(elemsize > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(length, UInt(elemsize.W)))
        val max = Output(UInt(elemsize.W))
        val pos = Output(UInt(log2Ceil(length).W))
    })

    if (length == 1) {
        // Base case. There is only one possible maximum for a Vec of size one.
        io.max := io.in(0)
        io.pos := 0.U
    } else {
        // Split up the input until their length is 1
        val length0 = if (isPow2(length)) length >> 1 else 1 << log2Floor(length) // Make the first length be a power of two >= length1
        val length1 = length - length0

        // Recursively create modules for the smaller sub-problems
        val vm0 = Module(new VecMax(length0, elemsize)).io
        val vm1 = Module(new VecMax(length1, elemsize)).io

        vm0.in := io.in.slice(0, length0)
        vm1.in := io.in.slice(length0, length)

        // Combine the output of the two modules
        when (vm0.max >= vm1.max) {
            io.max := vm0.max
            io.pos := vm0.pos
        }.otherwise {
            io.max := vm1.max
            // Since length0 is always a power of two, simply pad a 1 in front of the position
            if (length == 2) {
                io.pos := 1.U(1.W)
            } else {
                val posbits = io.pos.getWidth
                io.pos := Cat(1.U(1.W), Cat(0.U((posbits - 1).W), vm1.pos)(posbits - 2, 0))
            }
        }
    }

    override def desiredName = s"VecMax_${length}"
}

object VecMax extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new VecMax(length = 32, elemsize = 3)))
    )
}