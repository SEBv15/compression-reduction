package compression

import chisel3._
import chisel3.util._

import scala.collection.mutable.ListBuffer
import scala.math.pow

/** Merges many input vectors into one semi-continous word.
 *  
 *  By setting the maxblocks parameter, the reduction module limits the number of input elements a single stage (merge module) can have.
 *  Whenever the number would exceed the maxmimum number of blocks/elements, adjacent elements get merged together, which halves the number of elements.
 *  This possibly means grouping a used element with an unused one, reducing the compression ratio. 
 *
 *  @author Sebastian Strempfer
 *
 *  @param ninputs The number of input vectors
 *  @param numblocks The length of each input vector
 *  @param blockwidth The size of the UInt in the vector
 *  @param maxblocks The maximum number of blocks/elements/words any merge stage should have as input (0 = no limit)
 *  @param merge_weird Whether to use the MergeWeird module or the regular Merge
 */
class Reduction(val ninputs:Int = 64, val numblocks:Int = 10, val blockwidth:Int = 16, val maxblocks:Int = 128, val merge_weird:Boolean = true) extends Module {
    require(isPow2(ninputs))
    require(ninputs > 0)
    require(numblocks > 0)
    require(blockwidth > 0)
    require(maxblocks >= numblocks || maxblocks == 0)

    val io = IO(new Bundle {
        val in = Input(Vec(ninputs, Vec(numblocks, UInt(blockwidth.W))))
        val inlengths = Input(Vec(ninputs, UInt((log2Floor(numblocks) + 1).W)))
        val out = Output(Vec(ninputs*numblocks, UInt(blockwidth.W)))
        val outlength = Output(UInt((log2Floor(ninputs*numblocks) + 1).W))
    })

    // Make the first stage of mergers
    val stage1 = ListBuffer.fill(ninputs/2)(Module(new Merger(blockwidth, numblocks, numblocks, 0, 0, merge_weird)))
    for (i <- 0 until ninputs/2) {
        stage1(i).io.len1 := io.inlengths(2*i)
        stage1(i).io.len2 := io.inlengths(2*i+1)
        stage1(i).io.data1 := io.in(2*i)
        stage1(i).io.data2 := io.in(2*i+1)
    }

    // Use a list for all the other stages
    var stages:ListBuffer[ListBuffer[Merger]] = new ListBuffer[ListBuffer[Merger]]()
    stages.append(stage1)

    var nb = 2*numblocks; // number of blocks out of the first merge stage
    var div = 1; // what the number of blocks should be divided by relative to the number of blocks without a block limit

    // Add stages as needed and make them take the previous stage as input
    for (n <- 1 until log2Up(ninputs)) {
        var merge = false;
        if (nb > maxblocks && maxblocks != 0) {
            div *= 2;
            nb /= 2;
            merge = true;
        }
        stages.append(ListBuffer.fill(ninputs/pow(2, n+1).toInt)(Module(new Merger(blockwidth*div, numblocks*pow(2, n).toInt/div, numblocks*pow(2, n).toInt/div, 0, 0, merge_weird))))

        // If number of blocks needs to be divided, group two inputs together before feeding it into the next stage
        if (merge) {
            for (i <- 0 until ninputs/pow(2, n+1).toInt) {
                stages(n)(i).io.len1 := (stages(n-1)(2*i).io.outlen +& 1.U) / 2.U // Ceil division
                stages(n)(i).io.len2 := (stages(n-1)(2*i+1).io.outlen +& 1.U) / 2.U
                stages(n)(i).io.data1 := (0 until nb).map(x => Cat(stages(n-1)(2*i).io.out(2*x+1), stages(n-1)(2*i).io.out(2*x)))
                stages(n)(i).io.data2 := (0 until nb).map(x => Cat(stages(n-1)(2*i+1).io.out(2*x+1), stages(n-1)(2*i+1).io.out(2*x)))
            }
        } else {
            for (i <- 0 until ninputs/pow(2, n+1).toInt) {
                stages(n)(i).io.len1 := stages(n-1)(2*i).io.outlen
                stages(n)(i).io.len2 := stages(n-1)(2*i+1).io.outlen
                stages(n)(i).io.data1 := stages(n-1)(2*i).io.out
                stages(n)(i).io.data2 := stages(n-1)(2*i+1).io.out
            }
        }
        nb *= 2;
    }

    val data_uint = stages(stages.length - 1)(0).io.out.asUInt()
    val out = Wire(Vec(ninputs*numblocks, UInt(blockwidth.W)))
    out := (0 until ninputs*numblocks).map(x => data_uint(blockwidth*(x + 1) - 1, blockwidth*x))
    val out_reg = List.fill(ninputs*numblocks)(RegInit(0.U(blockwidth.W)))
    for (i <- 0 until out_reg.length) {
        out_reg(i) := out(i)
        io.out(i) := out_reg(i)
    }
    val outlength_reg = RegInit(0.U((log2Floor(ninputs*numblocks) + 1).W))
    outlength_reg := stages(stages.length - 1)(0).io.outlen * div.U
    io.outlength := outlength_reg
}

object Reduction extends App {
    chisel3.Driver.execute(args, () => new Reduction)
}
