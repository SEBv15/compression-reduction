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
object FullTestTag extends Tag("fullTest")

/** Test the whole compression & reduction stage by giving it data and checking if what comes out is the same as what we inserted
 *  This doesn't test any metadata or how the shifts are merged. Just if the pixels are correct.
 *  THIS IS VERY SLOW TO COMPILE (~3 minutes for every test)!
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
        //println(("INSERTED",  pixels.map(_.mkString).mkString(" ")))
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
    def get_blocks(c: CompressionReduction, print: Boolean = true) = {
        var blocks = new ArrayBuffer[BigInt]
        if (c.io.write_enable.peek().litValue == 1) {
            for (i <- 0 until c.io.blocks_used.peek().litValue.toInt) {
                var block = c.io.blocks(i).peek().litValue
                blocks += block
            }
        }
        if (blocks.length > 0 && print) {
            val numshifts = (blocks(0) >> (1024-8)) & ((1 << 7)-1)
            println("Received " + numshifts.toString() + " shift" + (if (numshifts == 1) "" else "s"))
        }
        blocks.toArray
    }

    /** Takes in the module output, reverses the entire compression (except for poisson encoding) and compares them to what is in the queue of inserted pixels.
     *
     *  This method doesn't have access to the compression module so if it can reverse the output to get back the original, it is guaranteed that the compression module output is usable.
     */
    def check_blocks(blocks: Array[BigInt], data_dropped: Boolean = false) {
        if (blocks.length == 0) return;
        val wlist = blocksToData(blocks, 16)
        //println(("BLOCKS AS RECEIVED 2", wlist.mkString(" ")))
        // Check the data in the blocks shift-by-shift
        val nmerged = (blocks(0) >> (1024-8)) & ((1 << 7) - 1)
        //println(nmerged)
        check_shift(wlist, nmerged.toInt, data_dropped)
    }

    /** Reverses the first compressed frame it finds and checks it against the input. Remaining data gets passed back to the function.
     */
    def check_shift(shifti: Array[BigInt], nleft: Int, data_dropped: Boolean) {
        var shift = shifti
        //println("Shift", shift.mkString(" "))
        // Since this function recursively calls itself to go through the shifts in the blocks, check for the base case where there is no data left.
        if (shift.length == 0 || nleft == 0) {
            return
        }

        if (shift.length < 20) {
            val big_zero: BigInt = 0
            shift = shift ++ Array.fill(20 - shift.length)(big_zero)
        }

        // Reverse the entire reduction and compression for the first shift in the data from the blocks
        val (headers, num_3bits) = getHeaders(shift)
        //println("HEADERS", headers.mkString(" "), num_3bits)

        var datalen = calculateReductionOutputLength(headers, 128, 7)
        //println("DATA PRE-MERGER", shift.slice(0, datalen + 64*5/16).mkString(" "))
        //datalen = 64
        //val red_out = reverseMergeWeird(shift.slice(64*2/16, shift.length), (num_3bits*3 + 15)/16, datalen, 64*3/16)
        //println("REDUCED", red_out.mkString(" "))

        var datastart = 64*2/16 + (num_3bits * 3 + 15)/16
        val data = reverseReduction(shift.slice(datastart, datalen + datastart), headers, 128, 7)
        //println("DATA", data.mkString(" "))

        //println("Headers", headers.mkString(" "))
        //println("Data", data.mkString(" "))

        val pixels = deshuffle(headers, data)

        //println("Pixels", (0 until pixels.length).map(i => pixels(i).mkString(" ")))

        // Check against what we have in the queue
        compare_data(pixels, data_dropped, nleft)

        // Do the same for the set of data after this one
        val num_header_blocks = 8 + (num_3bits*3 + 15)/16
        datalen += num_header_blocks
        datalen += (4 - (datalen % 4)) % 4
        shift = shift.slice(((datalen + 3)/4)*4, shift.length)
        check_shift(shift, nleft-1, data_dropped)
    }

    /** Takes in the reversed pixels and compares them against what is in the queue.
     * 
     *  If search is true, it will look through the frames in the pendings queue until it finds one that matches or runs out of frames (which will throw an error).
     */
    def compare_data(pixels: Array[Array[Int]], search: Boolean = false, nleft: Int = 0) {
        num_shifts_received += 1
        //println("Checking shift", num_shifts_received)
        //println(("CHECKING", pixels.map(_.mkString).mkString(" ")))
        var matches: Boolean = true
        var skipped: Int = -1

        var inserted_pixels = Array.fill(128)(Array.fill(8)(0))
        do {
            if (pendings.isEmpty) {
                throw new Exception("Reached the end of the queue")
            }

            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    inserted_pixels(i)(j) = pendings.dequeue
                }
            }

            matches = true
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    if (poissonEncode(inserted_pixels(i)(j)) != pixels(i)(j)) {
                        matches = false
                    }
                }
            }
            pending_shifts -= 1
            skipped += 1
        } while (!matches && search)

        if (skipped > 0) {
            println("skipped " + skipped + " shifts")
        }

        if (!matches) {
            println(pending_shifts)
            println(nleft)
            println(("CHECKING", pixels.map(_.mkString).mkString(" ")))
            println(("SHOULD BE", inserted_pixels.map(_.mkString).mkString(" ")))
            println(("SHOULD BE (POISSONED)", inserted_pixels.map(a => a.map(poissonEncode(_))).map(_.mkString).mkString(" ")))
        }
        assert(matches)
        //println(if (flag) "MATCHES!!!" else  "DOESN'T MATCH!!!")
    }

    it should "test with random data" taggedAs FullTestTag in {
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

            var data_dropped = false
            for (i <- 0 until 15) {
                // Get random pixels
                val data = generate_pixels(r)
                // for (j <- 0 until 128) {
                //     for (k <- 0 until 8) {
                //         data(j)(k) = i+1
                //     }
                // }
                val valid = i != 5 // skip the fifth frame to test data_valid

                val fifo_full = r.nextInt(5) == 0
                c.io.fifo_full.poke(fifo_full.B)

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, valid)

                if (c.io.data_dropped.peek().litValue == 1) data_dropped = true

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks, data_dropped)

                if (blocks.length > 0) data_dropped = false

                c.clock.step()
            }

            println("Soft Reset")
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

            for (i <- 0 until 200) {
                val data = generate_pixels(r)

                val valid = r.nextInt(5) != 0
                test_pixels(c, data, valid)

                val fifo_full = r.nextInt(5) == 0
                c.io.fifo_full.poke(fifo_full.B)

                if (c.io.data_dropped.peek().litValue == 1) data_dropped = true

                val blocks = get_blocks(c)
                check_blocks(blocks, data_dropped)

                if (blocks.length > 0) data_dropped = false

                c.clock.step()
            }
        }
    }

    it should "test with all zeros" taggedAs FullTestTag in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(0.U)
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

            for (i <- 0 until 160) {
                // Get random pixels
                val data = Array.fill(128)(Array.fill(8)(0))

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, true)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }

    it should "test with all ones" taggedAs FullTestTag in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()
            for (i <- 0 until 128) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(0.U)
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

            for (i <- 0 until 20) {
                // Get random pixels
                val data = Array.fill(128)(Array.fill(8)((1 << 10) - 1))

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, data, true)

                // Get the output from the module
                val blocks = get_blocks(c)
                check_blocks(blocks)

                c.clock.step()
            }
        }
    }

    it should "test compression bypass" taggedAs FullTestTag in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            pendings.clear()

            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(1.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            val r = new Random(1)

            for (i <- 0 until 20) {
                // Get random pixels
                val pixels = generate_pixels(r)

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                test_pixels(c, pixels, true)

                // fifo full signal really only affects the write enable and data dropped output
                val fifofull = (i % 4) == 0
                c.io.fifo_full.poke(fifofull.B)

                c.clock.step()

                c.io.blocks_used.expect(10.U)
                c.io.write_enable.expect((!fifofull).B)
                c.io.data_dropped.expect(fifofull.B)

                // Get the output from the module
                var data: BigInt = 0
                for (i <- 0 until 10) {
                    data <<= 1024
                    data += c.io.blocks(i).peek().litValue
                }
                val pixelso = Array.fill(128)(Array.fill(8)(0))
                for (i <- 0 until 128*8) {
                    pixelso(i / 8)(i % 8) = ((data >> (10*(128*8 - 1 - i))) & ((1 << 10) - 1)).intValue()
                    assert(pendings.dequeue == pixelso(i / 8)(i % 8))
                }
            }
        }
    }

    it should "test with real data" in {
        test(new CompressionReduction).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val frames = load_data_file("python-simulation/data/ptychography.bin")

            pendings.clear()
            num_shifts_received = 0

            c.io.fifo_full.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.frame_sync.poke(0.B)
            c.io.data_valid.poke(0.B)
            c.io.soft_rst.poke(0.B)

            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)

            c.io.data_valid.poke(1.B)

            var bits_received = 1
            var bits_processed = 0

            var s = true
            for (i <- 0 until frames.length) {
                for (j <- 0 until 16) {
                    //if (s && j < 6) {
                    //} else {
                    //    s = false
                    val pixels = Array.fill(128)(Array.fill(8)(0))
                    for (k <- 0 until 128) {
                        for (l <- 0 until 8) {
                            pixels(k)(l) = frames(i)(k)(l+j*8)
                        }
                    }

                    test_pixels(c, pixels, true)

                    val blocks = get_blocks(c)
                    check_blocks(blocks)

                    bits_received += blocks.length * 1024
                    bits_processed = num_shifts_received * 128*8*10

                    if (bits_received > 0) {
                        println("Compression Ratio", bits_processed.toDouble/bits_received, " - ", i*100/frames.length, "percent", i, j)
                    }

                    c.clock.step()
                    //}
                }
            }
        }
    }
}