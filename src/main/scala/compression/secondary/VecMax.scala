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
        val pos = Output(UInt(log2Ceil(length + 1).W))
    })

    if (length == 1) {
        io.max := io.in(0)
        io.pos := 0.U
    } else {
        val length0 = if (isPow2(length)) length >> 1 else 1 << log2Floor(length)
        val length1 = length - length0

        val fo0 = Module(new VecMax(length0, elemsize)).io
        val fo1 = Module(new VecMax(length1, elemsize)).io

        fo0.in := io.in.slice(0, length0)
        fo1.in := io.in.slice(length0, length)

        when (fo0.max >= fo1.max) {
            io.max := fo0.max
            io.pos := fo0.pos
        }.otherwise {
            io.max := fo1.max
            if (length == 2) {
                io.pos := 1.U(1.W)
            } else {
                io.pos := Cat(1.U(1.W), fo1.pos(log2Floor(length0) - 1, 0))
            }
        }
    }

    override def desiredName = s"VecMax_${length}"
}

object VecMax extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new VecMax(length = 32, elemsize = 16)))
    )
}