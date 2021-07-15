package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Takes in compressor output, reduces the data, and calculates the hierarchical headers. The output is the final data (before adding metadata).
 *  The output will look like this
 *  | 2-bit headers | variable sized 3-bit headers | variable sized data |
 *
 *  @author Sebastian Strempfer
 *
 *  @param ncompressors The number of compressors the module should take data from
 *  @param nelems Number of elements / words each compressor has
 *  @param elemsize Width of the elements / words the compressors give
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class HierarchicalReduction(ncompressors:Int = 64, nelems:Int = 10, elemsize:Int = 16, maxblocks:Int = 128) extends Module {
    require(isPow2(ncompressors))
    require(ncompressors > 8)
    require(nelems > 0)
    require(elemsize >= 2)
    require(isPow2(elemsize))
    require(elemsize <= 128)
    require(nelems > 3, "Hierarchical reduction is useless in this case")
    require(ncompressors*2 % elemsize == 0)

    val headerwidth = log2Floor(nelems) + 1
    val outsize = ncompressors*nelems + (ncompressors*2 + ncompressors*headerwidth + elemsize-1)/elemsize

    require(elemsize % headerwidth == 0, "This makes the code cleaner")

    val io = IO(new Bundle {
        val in = Input(Vec(ncompressors, new DynamicData(nelems, elemsize = elemsize)))
        val out = Output(new DynamicData(outsize, elemsize = elemsize))
    })

    // Make 2-bit headers
    val twobit_headers = Wire(Vec(ncompressors, UInt(2.W)))
    for (i <- 0 until ncompressors) {
        when (io.in(i).len >= 3.U) {
            twobit_headers(i) := 3.U
        }.otherwise {
            twobit_headers(i) := io.in(i).len
        }
    }
    val twobits_per_word = elemsize/2
    val twobit_headers_elemsize = (0 until ncompressors*2/elemsize).map(x => Cat(twobit_headers.slice(twobits_per_word*x, twobits_per_word*(x+1))))

    // Reduce full sized headers together
    val header_reduction = Module(new Reduction(ninputs = ncompressors, nelems = 1, elemsize = headerwidth, maxblocks = 0))
    for (i <- 0 until ncompressors) {
        header_reduction.io.in(i).data(0) := io.in(i).len
        header_reduction.io.in(i).len := io.in(i).len >= 3.U
    }

    val headers_elemsize = Wire(new DynamicData((ncompressors*headerwidth)/elemsize, elemsize = elemsize))
    val headers_per_word = elemsize/headerwidth
    headers_elemsize.data := (0 until ncompressors*headerwidth/elemsize).map(x => Cat(header_reduction.io.out.data.slice(headers_per_word*x, headers_per_word*(x+1))))
    headers_elemsize.len := (header_reduction.io.out.len*headerwidth.U +& (elemsize-1).U) / elemsize.U

    // Reduce data
    val data_reduction = Module(new Reduction(ninputs = ncompressors, nelems = nelems, elemsize = elemsize, maxblocks = maxblocks))
    data_reduction.io.in := io.in

    // Merge everything together
    val header_data_merger = Module(new Merge(wordsize = elemsize, inwords1 = headers_elemsize.data.length, inwords2 = ncompressors*nelems))
    header_data_merger.io.in1 := headers_elemsize
    header_data_merger.io.in2 := data_reduction.io.out

    io.out.data := twobit_headers_elemsize ++ header_data_merger.io.out.data
    io.out.len := (ncompressors*2/elemsize).U +& header_data_merger.io.out.len
}

object HierarchicalReduction extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new HierarchicalReduction))
    )
}
