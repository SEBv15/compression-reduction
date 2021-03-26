package compression

import org.scalatest._
import chiseltest._
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

/** Test the whole compression & reduction stage by giving it data and checking if what comes out is the same as what we inserted
 *  This doesn't test any metadata or how the shifts are merged. Just if the pixels are correct.
 *  THIS IS VERY SLOW!
 *
 *  @author Sebastian Strempfer
 */
class CompressionReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    var pendings = new Queue[Int]
    var pending_shifts = 0
    var num_shifts_received = 0

    def generate_pixels(r: Random) = {
        var data = ArrayBuffer.fill(128)(ArrayBuffer.fill(8)(0))
        for (i <- 0 until 128) {
            val numbits = r.nextInt(11) // just so we have a better chance of getting lower numbers
            for (j <- 0 until 8) {
                data(i)(j) = r.nextInt(1 << numbits)
            }
        }
        data
    }

    def test_pixels(c: CompressionReduction, pixels: ArrayBuffer[ArrayBuffer[Int]]) = {
        for (i <- 0 until 128) {
            for (j <- 0 until 8) {
                c.io.pixels(i)(j).poke(pixels(i)(j).U)
                pendings.enqueue(pixels(i)(j))
            }
        }
        pending_shifts += 1
    }

    def check_blocks(blocks: Array[BigInt]) {
        val wlist = blocksToData(blocks, 16)
        // Check the data in the blocks shift-by-shift
        check_shift(wlist)
    }

    def check_shift(shifti: Array[BigInt]) {
        var shift = shifti
        //println("Shift", shift.mkString(" "))
        // Since this function recursively calls itself to go through the shifts in the blocks, check for the base case where there is no data left.
        if (shift.length == 0 || shift(0) >> 8 == (1 << 8) - 1) {
            return
        }

        // Reverse the entire reduction and compression for the first shift in the data from the blocks
        val (headers, num_3bits) = getHeaders(shift)

        var datalen = calculateReductionOutputLength(headers, 128, 7)
        val red_out = reverseMergeWeird(shift.slice(64*2/16, shift.length), (num_3bits*3 + 15)/16, datalen, 64*3/16)

        val data = reverseWeirdReduction(red_out.slice(64*3/16, red_out.length), headers, 128, 7)

        //println("Headers", headers.mkString(" "))
        //println("Data", data.mkString(" "))

        val pixels = deshuffle(headers, data)

        //println("Pixels", (0 until pixels.length).map(i => pixels(i).mkString(" ")))

        // Check against what we have in the queue
        compare_data(pixels)

        // Do the same for the set of data after this one
        val num_header_blocks = 8 + (num_3bits*3 + 15)/16
        datalen += num_header_blocks
        datalen += (4 - (datalen % 4)) % 4
        shift = shift.slice(datalen, shift.length)
        check_shift(shift)
    }

    def deshuffle(headers: Array[Int], data: Array[BigInt]) = {
        var deshuffled = Array.fill(128)(Array.fill(8)(0))
        for (i <- 0 until 64) {
            for (j <- 0 until 16) {
                for (k <- 0 until headers(i)) {
                    deshuffled(i/2*4 + j/4)(j % 4 + 4*(i%2)) += (((data(7*i + k) >> j) & 1) << k).toInt
                }
            }
        }
        deshuffled
    }

    def compare_data(pixels: Array[Array[Int]]) {
        num_shifts_received += 1
        //println("Checking shift", num_shifts_received)
        for (i <- 0 until 128) {
            for (j <- 0 until 8) {
                assert(poissonEncode(pendings.dequeue) == pixels(i)(j))
            }
        }
        pending_shifts -= 1
    }

    it should "test-compression-reduction" in {
        test(new CompressionReduction) { c =>
            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.use_nth.poke(1.U)

            val r = new Random(1)

            for (i <- 0 until 10) {
                // Get random pixels
                var data = generate_pixels(r)
                /*for (i <- 0 until 128) {
                    for (j <- 0 until 8) {
                        data(i)(j) = 1
                    }
                }*/
                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data)

                //println("Inserted", data)

                /*val headers = Array.fill(64)(0)
                for (i <- 0 until 64) {
                    var max = 0
                    for (j <- 0 until 4) {
                        for (k <- 0 until 4) {
                            if (data(i/2*4+j)(k+4*(i % 2)) > max) {
                                max = data(i/2*4+j)(k+4*(i % 2))
                            }
                        }
                    }
                    if (max > 0)
                        headers(i) = log2Floor(poissonEncode(max)) + 1
                }
                println("Headers", headers.mkString(" ")) */

                // Get the output from the module
                var blocks = new ArrayBuffer[BigInt]
                for (i <- 0 until c.io.blocks_used.peek().litValue.toInt) {
                    blocks += c.io.blocks(i).peek().litValue
                }

                // Reverse the reduction and compression on the data and check against the input from the queue. 
                // Will fail the test case if the data is different or in the wrong order.
                check_blocks(blocks.toArray)

                c.clock.step()
            }
        }
    }
}