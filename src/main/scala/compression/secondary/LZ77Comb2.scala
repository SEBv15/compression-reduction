package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Use LZ77 compression to compress some input against some other data.
 *  
 *  Specifically this module tries to match prefixes in the data input to a sliding window in the compare_to input.
 *  To work the same as traditional LZ77 encoding, the compare_to input should be [window] bytes of the previous data and then the same as the data input.
 *  The maximum length of the prefix the module matches is determined by the bits left over, and is two for the default parameters (all the way at the bottom).
 *
 *  In the output, all elements will be [wordsize / 2 + 1] bits wide. The first two bits determine whether it is a literal or position length pair.  
 *  If it is a position length pair, the position indicates how many two bytes before the current one the prefix is from (0 = previous byte ...).
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The size of the input to LZ77 encode (needs to be a power of two)
 *  @param wordsize The size of the elements
 *  @param window The size of the window of data to search in (needs to be a power of two)
 */
class LZ77Comb2(length: Int, wordsize: Int, window: Int) extends Module {
    require(window > 0 && isPow2(window))
    require(length > 0)
    require(wordsize > 0 && isPow2(wordsize) && wordsize / 2 > log2Ceil(window))

    val input_bits = wordsize / 2 - 1 - log2Ceil(window)
    val input_size = 1 << input_bits

    val io = IO(new Bundle {
        val data = Input(Vec(length, UInt(wordsize.W)))
        val compare_to = Input(Vec(length + window, UInt(wordsize.W)))
        val out = Output(new DynamicData(length * 2, elemsize = wordsize / 2 + 1)) // Merged encoded data
        val encodings = Output(Vec(length, new LZ77Encoding(log2Ceil(window), input_bits))) // Just the encodings (for debug purposes)
    })

    // Find runs of matching data
    val runlengths = Wire(Vec(length, Vec(window, UInt((input_bits + 1).W))))
    for {
        i <- 0 until length
        j <- 0 until window
    } {
        when (io.compare_to(i + j) === io.data(i)) {
            if (i == length - 1)
                runlengths(i)(j) := 1.U
            else {
                val runlen = runlengths(i + 1)(j) + 1.U
                when (runlen(input_bits) === 1.U) {
                    runlengths(i)(j) := input_size.U // limit runlength to the maximum representable to prevent overflows
                }.otherwise { 
                    runlengths(i)(j) := runlen
                }
            }
        }.otherwise {
            runlengths(i)(j) := 0.U
        }
    }

    // LZ77 encoding for every element (including no encodings and overlaps)
    val encodings = Wire(Vec(length, new LZ77Encoding(log2Ceil(window), input_bits)))
    // Boolean array to indicate which encodings actually worked
    val valid_encoding_mask = Wire(Vec(length, Bool()))
    for (i <- 0 until length) {
        // Find the best prefix match for every element
        val vec_max = Module(new VecMax(window, input_bits + 1)).io
        vec_max.in := runlengths(i).reverse
        encodings(i).pos := vec_max.pos
        encodings(i).len := vec_max.max - 1.U // Subtract 1 since we don't need to encode no match
        valid_encoding_mask(i) := vec_max.max =/= 0.U
    }

    io.encodings := encodings

    // Inserting the encodings into the output
    val out = Wire(Vec(length * 2, new DynamicData(maxsize = 1, elemsize = wordsize / 2 + 1)))
    val encoded = Wire(Vec(length, UInt(input_bits.W)))
    for (i <- 0 until length) {
        val not_encoded = if (i > 0) encoded(i - 1) === 0.U else 1.B
        when (valid_encoding_mask(i) && not_encoded) {
            out(2*i).len := 1.U
            out(2*i).data(0) := Cat(1.U(2.W), encodings(i).pos, encodings(i).len)
            out(2*i + 1).len := 0.U
            encoded(i) := encodings(i).len
        }.otherwise {
            out(2*i).len := not_encoded
            out(2*i).data(0) := Cat(0.U(2.W), io.data(i)(wordsize - 1, wordsize / 2 + 1))
            out(2*i + 1).len := not_encoded
            if (i > 0)
                encoded(i) := encoded(i - 1) - (~not_encoded)
            else
                encoded(i) := 0.U
        }
        out(2*i + 1).data(0) := io.data(i)(wordsize / 2, 0)
    }

    var reduction = Module(new Reduction(ninputs = length * 2, nelems = 1, elemsize = wordsize / 2 + 1, maxblocks = 0)).io
    reduction.in := out
   
    io.out := reduction.out

    override def desiredName = s"LZ77Comb2_${length}"
}

object LZ77Comb2 extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new LZ77Comb2(length = 128, wordsize = 16, window = 32)))
    )
}
