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
 *  In the output, all elements will be [wordsize + 1] bits wide. The first bit determines whether it is a literal or position length pair.  
 *  If it is a position length pair, the position indicates how many bytes before the current one the prefix is from (0 = previous byte ...).
 *
 *  @author Sebastian Strempfer
 *
 *  @param length The size of the input to LZ77 encode (needs to be a power of two)
 *  @param wordsize The size of the elements
 *  @param window The size of the window of data to search in (needs to be a power of two)
 */
class LZ77Comb(length: Int, wordsize: Int, window: Int) extends Module {
    require(window > 0 && isPow2(window))
    require(length > 0)
    require(wordsize > 0 && wordsize > log2Ceil(window))

    val input_bits = wordsize - log2Ceil(window)
    val input_size = 1 << input_bits

    val io = IO(new Bundle {
        val data = Input(Vec(length, UInt(wordsize.W)))
        val compare_to = Input(Vec(length + window, UInt(wordsize.W)))
        val out = Output(new DynamicData(length, elemsize = wordsize + 1))
    })

    val prefix_mods = List.tabulate(length - input_size + 1)(i => Module(new LZ77Prefix(window = window, length = input_size, wordsize = wordsize)).io)
    for (i <- 0 to length - input_size) {
        prefix_mods(i).win := io.compare_to.slice(i, i + window)
        prefix_mods(i).input := io.data.slice(i, i + input_size)
    }

    val helper = Wire(Vec(length, UInt(input_bits.W)))
    val mask = Wire(Vec(length, Bool()))
    mask(0) := 1.B
    helper(0) := prefix_mods(0).len
    for (i <- 1 until length) {
        mask(i) := helper(i - 1) === 0.U
        when (helper(i - 1) =/= 0.U) {
            helper(i) := helper(i - 1) - 1.U
        }.otherwise {
            if (i > length - input_size) {
                helper(i) := 0.U
            } else {
                helper(i) := prefix_mods(i).len
            }
        }
    }

    val reduction = Module(new Reduction(ninputs = length, nelems = 1, elemsize = wordsize + 1, maxblocks = 0)).io
    for (i <- 0 until length) {
        if (i > length - input_size) {
            reduction.in(i).data(0) := Cat(0.U(1.W), io.data(i))
        } else {
            when (prefix_mods(i).found && prefix_mods(i).len =/= 0.U) {
                reduction.in(i).data(0) := Cat(1.U(1.W), prefix_mods(i).pos, prefix_mods(i).len)
            }.otherwise {
                reduction.in(i).data(0) := Cat(0.U(1.W), io.data(i))
            }
        }
        reduction.in(i).len := mask(i)
    }

    io.out := reduction.out

    override def desiredName = s"LZ77Comb_${length}"
}

object LZ77Comb extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new LZ77Comb(length = 256, wordsize = 8, window = 64)))
    )
}
