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

    def test_pixels(c: CompressionReduction, pixels: ArrayBuffer[ArrayBuffer[Int]], enqueue: Boolean = true) = {
        for (i <- 0 until 128) {
            for (j <- 0 until 8) {
                c.io.pixels(i)(j).poke(pixels(i)(j).U)
                if (enqueue) {
                    pendings.enqueue(pixels(i)(j))
                }
            }
        }
        pending_shifts += 1
    }

    def check_blocks(blocks: Array[BigInt]) {
        val wlist = blocksToData(blocks, 16)
        //println(("BLOCKS AS RECEIVED 2", wlist.mkString(" ")))
        // Check the data in the blocks shift-by-shift
        val nmerged = (blocks(0) >> (1024-8)) & ((1 << 7) - 1)
        println(nmerged)
        check_shift(wlist, nmerged.toInt)
    }

    def check_shift(shifti: Array[BigInt], nleft: Int) {
        var shift = shifti
        //println("Shift", shift.mkString(" "))
        // Since this function recursively calls itself to go through the shifts in the blocks, check for the base case where there is no data left.
        if (shift.length == 0 || nleft == 0) {
            return
        }

        // Reverse the entire reduction and compression for the first shift in the data from the blocks
        val (headers, num_3bits) = getHeaders(shift)
        //println("HEADERS", headers.mkString(" "), num_3bits)

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

    it should "test-compression-reduction" in {
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
                for (j <- 0 until 128) {
                    for (k <- 0 until 8) {
                        data(j)(k) = i+1
                    }
                }
                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                val valid = i != 5
                c.io.data_valid.poke(valid.B)
                //println("Datavalid", valid)
                // if (!pendings.isEmpty)
                //     println(pendings.last)
                test_pixels(c, data, valid)
                // if (!pendings.isEmpty)
                //     println(pendings.last)

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

                //println(("REDUCER LEN", c.io.red_len.peek().litValue))
                //println(("BUF SIZE", c.io.buf_size.peek().litValue))

                // Get the output from the module
                if (c.io.write_enable.peek().litValue == 1) {
                    var blocks = new ArrayBuffer[BigInt]
                    var s : String = ""
                    val big_one : BigInt = 1
                    for (i <- 0 until c.io.blocks_used.peek().litValue.toInt) {
                        var block = c.io.blocks(i).peek().litValue
                        blocks += block
                        println(c.io.blocks(i).peek().litValue >> (1024-8))
                        for (j <- 0 until 1024) {
                            if (((block >> (1023 - j)) & 1) == big_one) {
                                s += "1"
                            } else {
                                s += "0"
                            }
                        }
                    }
                    println("GOT", blocks.length, "blocks")
                    println("-")

                    //println(("BLOCKS AS RECEIVED", s))
                    //println(("BLOCKS AS RECEIVED", blocks.map(toBinary(_, 1024))))

                    // Reverse the reduction and compression on the data and check against the input from the queue. 
                    // Will fail the test case if the data is different or in the wrong order.
                    if (blocks.length > 0) {
                        check_blocks(blocks.toArray)
                    }
                }

                c.clock.step()
            }

            c.io.soft_rst.poke(1.B)
            c.clock.step()
            c.clock.step()
            c.io.soft_rst.poke(0.B)
            c.io.frame_sync.poke(1.B)
            c.io.data_valid.poke(0.B)
            c.clock.step()
            c.io.frame_sync.poke(0.B)
            pendings.clear()

            for (i <- 0 until 10) {
                // Get random pixels
                var data = generate_pixels(r)

                // Insert the pixels into the CompressionReduction module (also adds them to a queue which the output will be checked against)
                c.io.data_valid.poke((!(i == 5 || i == 6)).B)
                test_pixels(c, data, !(i==5 || i==6))

                if (c.io.write_enable.peek().litValue == 1) {
                    var blocks = new ArrayBuffer[BigInt]
                    var s : String = ""
                    val big_one : BigInt = 1
                    for (i <- 0 until c.io.blocks_used.peek().litValue.toInt) {
                        var block = c.io.blocks(i).peek().litValue
                        blocks += block
                        println(c.io.blocks(i).peek().litValue >> (1024-8))
                        for (j <- 0 until 1024) {
                            if (((block >> (1023 - j)) & 1) == big_one) {
                                s += "1"
                            } else {
                                s += "0"
                            }
                        }
                    }
                    println("GOT", blocks.length, "blocks")
                    println("-")

                    //println(("BLOCKS AS RECEIVED", s))
                    //println(("BLOCKS AS RECEIVED", blocks.map(toBinary(_, 1024))))

                    // Reverse the reduction and compression on the data and check against the input from the queue. 
                    // Will fail the test case if the data is different or in the wrong order.
                    if (blocks.length > 0) {
                        check_blocks(blocks.toArray)
                    }
                }

                c.clock.step()
            }
        }
    }
}