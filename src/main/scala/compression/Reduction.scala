package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

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
 *  @param nelems The length of each input vector
 *  @param elemsize The size of the UInt in the vector
 *  @param maxblocks The maximum number of blocks/elements/words any merge stage should have as input (0 = no limit)
 */
class Reduction(ninputs:Int = 64, nelems:Int = 10, elemsize:Int = 16, maxblocks:Int = 128) extends Module {
    require(isPow2(ninputs))
    require(ninputs > 0)
    require(nelems > 0)
    require(elemsize > 0)
    require(maxblocks >= nelems || maxblocks == 0)

    val io = IO(new Bundle {
        val in = Input(Vec(ninputs, new DynamicData(nelems, elemsize = elemsize)))
        val out = Output(new DynamicData(ninputs*nelems, elemsize = elemsize))
    })

    var stages: ListBuffer[List[Merge]] = new ListBuffer[List[Merge]]()

    stages += List.fill(ninputs/2)(Module(new Merge(wordsize = elemsize, inwords1 = nelems, inwords2 = nelems)))
    for (i <- 0 until ninputs/2) {
        stages(0)(i).io.in1 := io.in(2*i)
        stages(0)(i).io.in2 := io.in(2*i+1)
    }

    var granularity = 1
    for (n <- 1 until log2Ceil(ninputs)) {
        if (maxblocks < nelems*(1 << n) / granularity && maxblocks != 0) {
            granularity *= 2
        }

        stages += List.fill(ninputs/(1 << n + 1))(Module(new Merge(wordsize = elemsize, inwords1 = nelems*(1 << n), inwords2 = nelems*(1 << n), granularity = granularity)))

        for (i <- 0 until stages(n).length) {
            stages(n)(i).io.in1 := stages(n - 1)(2 * i).io.out
            stages(n)(i).io.in2 := stages(n - 1)(2 * i + 1).io.out
        }
    }

    io.out := stages(stages.length - 1)(0).io.out

    override def desiredName = s"Reduction_${ninputs}x$nelems"
}

object Reduction extends App {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new Reduction))
    )
}
