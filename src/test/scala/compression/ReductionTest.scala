package compression

import org.scalatest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chisel3._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import testUtils._

/** Tests the Reduction module by inputting randomly generated numbers, reversing the output in scala, and checking it against the input.
 *
 *  @author Sebastian Strempfer
 */
class ReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    val ninputs = 64
    val nelems = 7
    val elemwidth = 16
    val maxblocks = 128

    it should "test reduction random" /*taggedAs UnitTestTag*/ in {
        test(new Reduction(ninputs, nelems, elemwidth, maxblocks)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val r = new Random(1) // remove the seed to get a completely random test. It is there to make test case failures reproducable.
            val maxval = (1 << elemwidth) - 1

            // Run the test for 10 different inputs
            for (_ <- 0 until 10) {
                val lens = new Array[Int](ninputs)
                val inp = Array.fill(ninputs)(Array.fill(nelems)(0))

                // Generate random input data and give it to the module
                for (i <- 0 until ninputs) {
                    lens(i) = r.nextInt(nelems+1)
                    c.io.inlengths(i).poke(lens(i).U)
                    for (j <- 0 until lens(i))
                        inp(i)(j) = r.nextInt(maxval+1)
                    for (j <- 0 until nelems)
                        c.io.in(i)(j).poke(inp(i)(j).U)
                }

                // Get the output and put it into an array
                val outlen = c.io.outlength.peek().litValue.toInt
                val out = new Array[BigInt](outlen)
                for (i <- 0 until outlen) {
                    out(i) = c.io.out(i).peek().litValue
                }

                // Reverse the reduction on the output
                val reversed = reverseWeirdReduction(out, lens, maxblocks, nelems)

                // Check if the input data is the same as the reversed output
                for (i <- 0 until ninputs)
                    for (j <- 0 until lens(i))
                        assert(reversed(nelems*i+j) == inp(i)(j))
            }
        }
    }
    it should "test reduction single element" in {
        test(new Reduction(ninputs, nelems, elemwidth, maxblocks)) { c =>
            for (i <- 0 until ninputs) {
                for (j <- 0 until nelems) {
                    c.io.in(i)(j).poke(0.U)
                }
            }
            c.io.in(0)(0).poke(1.U)
            
            c.io.inlengths(0).poke(1.U)
            for (i <- 1 until ninputs) {
                c.io.inlengths(i).poke(0.U)
            }

            c.io.out(0).expect(1.U)
            for (i <- 1 until ninputs*nelems) {
                c.io.out(i).expect(0.U)
            }
            c.io.outlength.expect(2.U)
        }
    }
}