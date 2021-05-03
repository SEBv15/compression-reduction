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

/** Test the whole compression & reduction stage by giving it data and checking if what comes out is the same as what we inserted
 *  This doesn't test any metadata or how the shifts are merged. Just if the pixels are correct.
 *  THIS IS VERY SLOW!
 *
 *  @author Sebastian Strempfer
 */
class CompressionReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    var pendings = new Queue[Int] // queue holds inserted pixels in row-major order
    var pending_shifts = 0 // how many shifts are still in the queue (still buffering in the ensureblocks module or pipeline)
    var num_shifts_received = 0 // Number of shifts that have been processed out of the compression block

    /** Generates a frames worth of random pixels
     *
     *  Each frame is capped at a random number of bits so we see more variety in compression efficiency. 
     *  Also each 16-pixel block is capped by another random number of bits.
     */
    def generate_pixels(r: Random) = {
        var data = Array.fill(128)(Array.fill(8)(0))
        val framebits = r.nextInt(11)
        for (i <- 0 until 128 by 2) {
            val numbits = r.nextInt(framebits+1)
            for (j <- 0 until 2) {
                for (k <- 0 until 8) {
                    data(i+j)(k) = r.nextInt(1 << numbits)
                }
            }
        }
        data
    }

    /** Inserts an array of pixels into the compression module
     *
     *  The valid parameter signals whether the data is valid and should therefore be added to the queue to check the output with
     */
    def test_pixels(c: CompressionReduction, pixels: Array[Array[Int]], valid: Boolean = true) = {
        println(("INSERTED",  pixels.map(_.mkString).mkString(" ")))
        for (i <- 0 until 128) {
            for (j <- 0 until 8) {
                c.io.data_valid.poke(valid.B)
                c.io.pixels(i)(j).poke(pixels(i)(j).U)
                if (valid) {
                    pendings.enqueue(pixels(i)(j))
                }
            }
        }
        pending_shifts += 1
    }

    /** Reads the blocks from the compression module and returns them as an array of bigints
     */
    def get_blocks(c: CompressionReduction) = {
        var blocks = new ArrayBuffer[BigInt]
        if (c.io.write_enable.peek().litValue == 1) {
            for (i <- 0 until c.io.blocks_used.peek().litValue.toInt) {
                var block = c.io.blocks(i).peek().litValue
                blocks += block
            }
        }
        blocks.toArray
    }

    /** Takes in the module output, reverses the entire compression (except for poisson encoding) and compares them to what is in the queue of inserted pixels.
     *
     *  This method doesn't have access to the compression module so if it can reverse the output to get back the original, it is guaranteed that the compression module output is usable.
     */
    def check_blocks(blocks: Array[BigInt]) {
        if (blocks.length == 0) return;
        val wlist = blocksToData(blocks, 16)
        //println(("BLOCKS AS RECEIVED 2", wlist.mkString(" ")))
        // Check the data in the blocks shift-by-shift
        val nmerged = (blocks(0) >> (1024-8)) & ((1 << 7) - 1)
        println(nmerged)
        check_shift(wlist, nmerged.toInt)
    }

    /** Reverses the first compressed frame it finds and checks it against the input. Remaining data gets passed back to the function.
     */
    def check_shift(shifti: Array[BigInt], nleft: Int) {
        var shift = shifti
        //println("Shift", shift.mkString(" "))
        // Since this function recursively calls itself to go through the shifts in the blocks, check for the base case where there is no data left.
        if (shift.length == 0 || nleft == 0) {
            return
        }

        // Reverse the entire reduction and compression for the first shift in the data from the blocks
        val (headers, num_3bits) = getHeaders(shift)
        println("HEADERS", headers.mkString(" "), num_3bits)

        var datalen = calculateReductionOutputLength(headers, 128, 7)
        //println("DATA PRE-MERGER", shift.slice(0, datalen + 64*5/16).mkString(" "))
        //datalen = 64
        val red_out = reverseMergeWeird(shift.slice(64*2/16, shift.length), (num_3bits*3 + 15)/16, datalen, 64*3/16)
        //println("REDUCED", red_out.mkString(" "))

        val data = reverseWeirdReduction(red_out.slice(64*3/16, red_out.length), headers, 128, 7)
        //println("DATA", data.mkString(" "))

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
        shift = shift.slice(((datalen + 3)/4)*4, shift.length)
        check_shift(shift, nleft-1)
    }

    /** Takes in the reversed pixels and compares them against what is in the queue
     */
    def compare_data(pixels: Array[Array[Int]]) {
        num_shifts_received += 1
        //println("Checking shift", num_shifts_received)
        println(("CHECKING", pixels.map(_.mkString).mkString(" ")))
        var flag: Boolean = true
        for (i <- 0 until 128) {
            for (j <- 0 until 8) {
                val penc = poissonEncode(pendings.dequeue)
                assert(penc == pixels(i)(j))
                if (!(penc == pixels(i)(j))) flag = false
            }
        }
        println(if (flag) "MATCHES!!!" else  "DOESN'T MATCH!!!")
        pending_shifts -= 1
    }

    it should "test compression reduction with random data" in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(10.U)
                }
            }
            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            c.io.data_valid.poke(1.B)

            val r = new Random(1)

            for (i <- 0 until 15) {
                // Get random pixels
                var data = generate_pixels(r)
                // for (j <- 0 until 128) {
                //     for (k <- 0 until 8) {
                //         data(j)(k) = i+1
                //     }
                // }
                val valid = i != 5 // skip the fifth frame to test data_valid

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, valid)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }

            // soft reset
            c.io.soft_rst.poke(1.B)
            c.clock.step()
            c.clock.step()
            c.io.soft_rst.poke(0.B)
            pendings.clear()

            // frame sync
            c.io.frame_sync.poke(1.B)
            c.io.data_valid.poke(0.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            for (i <- 0 until 10) {
                // Get random pixels
                var data = generate_pixels(r)

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                val valid = !(i == 5 || i == 6)
                test_pixels(c, data, valid)

                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }

    it should "test compression reduction with half 1 half 0" in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(10.U)
                }
            }
            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            c.io.data_valid.poke(1.B)

            val r = new Random(1)

            for (i <- 0 until 30) {
                // Get random pixels
                var data = generate_pixels(r)
                for (i <- 0 until 128) {
                    for (j <- 0 until 8) {
                        if (i < 64) {
                            data(i)(j) = 1
                        } else {
                            data(i)(j) = 0
                        }
                    }
                }
                val valid = true

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, valid)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }

    it should "test compression reduction with half 1 half 0 horizontal" in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(10.U)
                }
            }
            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            c.io.data_valid.poke(1.B)

            val r = new Random(1)

            for (i <- 0 until 30) {
                // Get random pixels
                var data = generate_pixels(r)
                for (i <- 0 until 128) {
                    for (j <- 0 until 8) {
                        if (j < 4) {
                            data(i)(j) = 1
                        } else {
                            data(i)(j) = 0
                        }
                    }
                }
                val valid = true

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, valid)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }
    it should "test compression reduction with half 1 half 0 random" in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(10.U)
                }
            }
            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            c.io.data_valid.poke(1.B)

            val r = new Random(1)

            for (i <- 0 until 30) {
                // Get random pixels
                var data = generate_pixels(r)
                for (i <- 0 until 128) {
                    val v = r.nextInt(2)
                    for (j <- 0 until 8) {
                        data(i)(j) = r.nextInt(v+1)
                    }
                }
                val valid = true

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, valid)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }
}