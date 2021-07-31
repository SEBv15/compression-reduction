package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Searches for a prefix of the input in the given window of data and returns the position and length of the match.
 *
 *  @author Sebastian Strempfer
 *
 *  @param window The size of the window of data to search in (needs to be a power of two)
 *  @param length The size of the input to find the prefix for (needs to be a power of two)
 *  @param wordsize The size of the elements
 */
class LZ77Prefix(window: Int, length: Int, wordsize: Int) extends Module {
    require(window > 0 && isPow2(window))
    require(length > 0 && isPow2(length))
    require(wordsize > 0)

    val io = IO(new Bundle {
        val win = Input(Vec(window, UInt(wordsize.W)))
        val input = Input(Vec(length, UInt(wordsize.W)))
        val pos = Output(UInt(log2Ceil(window).W))
        val len = Output(UInt(log2Ceil(length).W))
        val found = Output(Bool())
    })

    val masks = List.tabulate(length)(i => Wire(Vec(window - i, Bool())))
    for (j <- 0 until window) {
        masks(0)(j) := io.win(j) === io.input(0)
    }
    for {
        i <- 1 until length
        j <- 0 until window - i
    } {
        masks(i)(j) := masks(i - 1)(j) && io.win(j + i) === io.input(i)
    }

    val position_mods = List.tabulate(length)(i => Module(new LZ77FirstOne(window - i)).io)
    for (i <- 0 until length) {
        position_mods(i).in := masks(i).reverse
    }

    val positions = Wire(Vec(length, UInt(log2Ceil(window).W)))
    val lengths = Wire(Vec(length, UInt(log2Ceil(length).W)))
    positions(0) := position_mods(0).out
    lengths(0) := 0.U
    io.found := position_mods(0).out =/= window.U
    for (i <- 1 until length) {
        when (position_mods(i).out =/= (window - i).U) {
            positions(i) := position_mods(i).out
            lengths(i) := i.U
        }.otherwise {
            positions(i) := positions(i - 1)
            lengths(i) := lengths(i - 1)
        }
    }

    io.len := lengths(length - 1)
    io.pos := positions(length - 1) +& lengths(length - 1)

    override def desiredName = s"LZ77Prefix_${window}_${length}"
}

class LZ77FirstOne(length: Int) extends Module {
    require(length > 0)

    val io = IO(new Bundle {
        val in = Input(Vec(length, Bool()))
        val out = Output(UInt(log2Ceil(length + 1).W))
    })

    if (length == 1) {
        io.out := ~io.in(0)
    } else {
        val length0 = if (isPow2(length)) length >> 1 else 1 << log2Floor(length)
        val length1 = length - length0

        val fo0 = Module(new LZ77FirstOne(length0)).io
        val fo1 = Module(new LZ77FirstOne(length1)).io

        fo0.in := io.in.slice(0, length0)
        fo1.in := io.in.slice(length0, length)

        when (fo0.out(fo0.out.getWidth - 1) === 0.U) {
            io.out := fo0.out
        }.otherwise {
            io.out := (1 << log2Ceil(length) - 1).U +& fo1.out
        }
    }

    override def desiredName = s"LZ77FirstOne_${length}"
}

object LZ77Prefix extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new LZ77Prefix(window = 64, length = 4, wordsize = 8)))
    )
}
