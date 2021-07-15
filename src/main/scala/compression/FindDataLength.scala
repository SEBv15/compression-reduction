package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Finds the length of data in a Vec by determining the location of the last non-zero element in a recursive manner
 *
 *  @author Sebastian Strempfer
 *
 *  @param nelems Number of pixels to compress together
 *  @param elemsize Number of bits per pixel 
 */
class FindDataLength(nelems:Int = 10, elemsize:Int = 16) extends Module {
    require(nelems > 0)
    require(elemsize > 0)

    val io = IO(new Bundle {
        val in  = Input(Vec(nelems, UInt(elemsize.W)))
        val len = Output(UInt(log2Ceil(nelems + 1).W))
    })

    if (nelems == 1) {
        io.len := io.in(0) =/= 0.U
    } else {
        val size0 = if (isPow2(nelems)) nelems >> 1 else 1 << log2Floor(nelems)
        val size1 = nelems - size0

        val fdl0 = Module(new FindDataLength(size0, elemsize))
        val fdl1 = Module(new FindDataLength(size1, elemsize))

        fdl0.io.in := io.in.slice(0, size0)
        fdl1.io.in := io.in.slice(size0, size0 + size1)

        when (fdl1.io.len === 0.U) {
            io.len := fdl0.io.len
        }.otherwise {
            io.len := size0.U +& fdl1.io.len
        }
    }

    override def desiredName = s"FindDataLength_${nelems}"
}

object FindDataLength extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new FindDataLength))
    )
}