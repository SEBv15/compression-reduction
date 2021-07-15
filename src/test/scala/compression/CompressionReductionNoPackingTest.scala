package compression

import org.scalatest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chisel3._
import chisel3.util.log2Floor

import scala.math.pow
import scala.math.floor
import scala.math.max

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

import scala.util.Random

import testUtils._

import org.scalatest.Tag

/** Test the whole compression & reduction stage by giving it data and checking if what comes out is the same as what we inserted
 *
 *  @author Sebastian Strempfer
 */
class CompressionReductionNoPackingTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "CompressionReductionNoPacking" should "work with random data" taggedAs UnitTestTag in {
        test(new CompressionReductionNoPacking).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val r = new Random(1)
            for (n <- 0 until 100) {
                val poisson: Boolean = r.nextInt(2) == 1
                c.io.poisson.poke(poisson.B)

                val pixels = generate_pixels(r)
                for (i <- 0 until 128) {
                    for (j <- 0 until 8) {
                        c.io.pixels(i)(j).poke(pixels(i)(j).U)
                    }
                }

                val datalen = c.io.out.len.peek().litValue.toInt
                val data: Array[BigInt] = Array.fill(64*(6+16*10) / 16)(0)
                for (i <- 0 until datalen) {
                    data(i) = c.io.out.data(i).peek().litValue
                }

                val (headers, num_4bits) = getHeaders(data, 64, 4, 16)
                val outr = reverseReduction(data.drop(64*2/16 + (num_4bits*4 + 15)/16), headers, 128, 10)
                val outp = deshuffle(headers, outr, 10)

                for (i <- 0 until 128) {
                    for (j <- 0 until 8) {
                        if (poisson) {
                            assert(outp(i)(j) == poissonEncode(pixels(i)(j)))
                        } else {
                            assert(outp(i)(j) == pixels(i)(j))
                        }
                    }
                }

                c.clock.step()
            }
        }
    }
}