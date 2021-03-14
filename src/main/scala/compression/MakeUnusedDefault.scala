package compression

import chisel3._
import chisel3.util._

import scala.math.pow

/** The merge modules don't have a clearly defined behaviour for values outside the returned length to converse area. 
 *  At the end of all merging, this module can be used to ensure that values after the used elements are the default value of all 1s.
 *  This is important since the decoding algorithm will probably look for non all 1s to determine if there is data.
 *
 *  @author Sebastian Strempfer
 *
 *  @param numwords The size of the vector
 *  @param wordsize The bit width of the elements
 */
class MakeUnusedDefault(val numwords:Int = 10, val wordsize:Int = 16) extends Module {
    require(wordsize > 0)
    require(numwords > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(numwords, UInt(wordsize.W)))
        val inlen = Input(UInt((log2Floor(numwords)+1).W))
        val out = Output(Vec(numwords, UInt(wordsize.W)))
        val outlen = Output(UInt((log2Floor(numwords)+1).W))
    })

    var defaultval: BigInt = 1
    defaultval = (defaultval << wordsize) - 1

    for (i <- 0 until numwords) {
        io.out(i) := Mux(io.inlen > i.U, io.in(i), defaultval.U)
    }

    io.outlen := io.inlen
}

object MakeUnusedDefault extends App {
    chisel3.Driver.execute(args, () => new MakeUnusedDefault)
}
