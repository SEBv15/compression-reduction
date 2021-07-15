package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.math.floor
import scala.math.max

import scala.util.Random

import testUtils._

/** Tests the HierarchicalReduction module by inputting randomly generated numbers, reversing the output in scala, and checking it against the input.
 *
 *  @author Sebastian Strempfer
 */
class HierarchicalReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    // Number of compressors to test the reduction stage with (32 is max for me before running out of memory)
    val ncompressors = 16
    val nelems = 10
    val maxblocks = 128

    "HierarchicalReduction" should "work correctly" taggedAs UnitTestTag in {
        // test case body here
        test(new HierarchicalReduction(ncompressors, nelems, 16, maxblocks)) { c =>
            val r = new Random(1) // remove the seed to get a completely random test. It is there to make test case failures reproducable.
            val maxval = (1 << 16) - 1

            for (_ <- 0 until 10) {
                val lens = new Array[Int](ncompressors)
                val inp = Array.fill(ncompressors)(Array.fill(nelems)(0))

                // Generate random input data and give it to the module
                for (i <- 0 until ncompressors) {
                    lens(i) = r.nextInt(nelems+1)
                    c.io.in(i).len.poke(lens(i).U)
                    for (j <- 0 until lens(i))
                        inp(i)(j) = r.nextInt(maxval+1)
                    for (j <- 0 until nelems)
                        c.io.in(i).data(j).poke(inp(i)(j).U)
                }

                // Get the output and put it into an array
                var outlen = c.io.out.len.peek().litValue.toInt
                val out = new Array[BigInt](outlen)
                for (i <- 0 until outlen) {
                    out(i) = c.io.out.data(i).peek().litValue
                }

                val (headers, num_4bits) = getHeaders(out, ncompressors, 4, 16)

                //val red_out = reverseMergeWeird(out.slice(ncompressors*2/16, out.length), (num_3bits*3 + 15)/16, calculateReductionOutputLength(headers, maxblocks, nelems), (ncompressors*3 + 15)/16)
                val red_out = out.slice(ncompressors*2/16 + (num_4bits*4 + 15)/16, out.length)

                val reversed = reverseReduction(red_out, headers, maxblocks, nelems)

                // println(lens.mkString(" "))
                // println(headers.mkString(" "))
                // println(out.slice(ncompressors*2/16, ncompressors*2/16 + 3).mkString(" "))
                // println(num_4bits)
                // println(reversed.map(e => f"${e}%05d").mkString(" "))
                // println(inp.map(_.map(e => f"${e}%05d").mkString(" ")).mkString("\n"))
                // Check if the input data is the same as the reversed output
                for (i <- 0 until ncompressors)
                    for (j <- 0 until lens(i))
                        assert(reversed(nelems*i+j) == inp(i)(j))
            }
        }
    }
}
