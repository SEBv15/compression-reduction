package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Coalescing module as described in the XLOOP paper
 *
 *  @author Sebastian Strempfer
 *
 *  @param pixel_rows The number of pixel rows
 *  @param pixel_cols The number of pixel columns (currently required to be 8)
 *  @param maxblocks Same as the maxblocks parameter in Reduction. Limits the granularity of the data reduction.
 */
class Coalescing(num_inputs: Int = 64, input_maxsize: Int = 10, input_elemwidth: Int = 16, input_metadatawidth: Int = 4) extends Module {
    require(num_inputs * input_metadatawidth % input_elemwidth == 0, "Metadata must be able to pack into elements")

    val io = IO(new Bundle {
        // The variable-length part of the input
        val inputs = Input(Vec(num_inputs, new DynamicData(input_maxsize, elemsize = input_elemwidth)))

        // The fixed-size part of the input
        val input_metadata = Input(Vec(num_inputs, UInt(input_metadatawidth.W)))

        // The output which only contains valid data when write_enable is true
        val output = Output(UInt((num_inputs * (input_maxsize * input_elemwidth + input_metadatawidth)).W))

        // Indicates when to store the output of the module
        val write_enable = Output(Bool())
    })

    val reduction = Module(new Reduction(
        ninputs = num_inputs, 
        nelems = input_maxsize, 
        elemsize = input_elemwidth, 
        maxblocks = 0
    )).io
    reduction.in := io.inputs

    val meta_per_elem = input_elemwidth / input_metadatawidth
    val metadata_size = num_inputs / meta_per_elem
    val concat_size = num_inputs * input_maxsize + metadata_size

    val concatenated = Wire(new DynamicData(concat_size, elemsize = input_elemwidth))

    // Put the concatenated metadata at the beginning
    concatenated.data.slice(0, metadata_size)
    .zip((0 until metadata_size).map(i => Cat(io.input_metadata.slice(i * meta_per_elem, (i + 1) * meta_per_elem))))
    .foreach { case (a, b) => a:= b }

    // And the reduction output at the end
    concatenated.data.slice(metadata_size, concat_size).zip(reduction.out.data).foreach { case (a, b) => a:= b }

    concatenated.len := metadata_size.U +& reduction.out.len

    val packing = Module(new PaperPacker(
        maxsize = concat_size,
        elemwidth = input_elemwidth
    )).io
    packing.input := concatenated

    io.output := packing.output
    io.write_enable := packing.write_enable
}

/** Simplified version of the Packer module
 *
 *  This version is the basic implementation as described and pictured in the XLOOP paper
 *
 *  @author Sebastian Strempfer
 *
 *  @param maxsize Maximum number of input elements per clock cycle
 *  @param elemwidth Width of the individual elements in the input
 */
class PaperPacker(maxsize: Int = 656, elemwidth: Int = 16) extends Module {
    val io = IO(new Bundle {
        val input = Input(new DynamicData(maxsize, elemsize = elemwidth))
        val output = Output(UInt((maxsize * elemwidth).W))
        val write_enable = Output(Bool())
    })

    val buffer = RegInit(0.U.asTypeOf(new DynamicData(maxsize, elemsize = elemwidth)))

    val merger = Module(new Merge(wordsize = elemwidth, granularity = 1, inwords1 = maxsize, inwords2 = maxsize)).io
    merger.in1 := buffer
    merger.in2 := io.input

    when (merger.out.len >= maxsize.U) {
        io.write_enable := 1.B
        buffer.data := merger.out.data.slice(maxsize, 2*maxsize)
        buffer.len := merger.out.len - maxsize.U
    }.otherwise {
        io.write_enable := 0.B
        buffer.data := merger.out.data.slice(0, maxsize)
        buffer.len := merger.out.len
    }

    io.output := Cat(merger.out.data.slice(0, maxsize))
}

object Coalescing extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new Coalescing))
    )
}