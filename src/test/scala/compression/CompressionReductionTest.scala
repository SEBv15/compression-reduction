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
 *  This doesn't test any metadata or how the shifts are merged. Just if the pixels are correct.
 *  THIS IS VERY SLOW TO COMPILE (~3 minutes for every test)!
 *
 *  @author Sebastian Strempfer
 */
class CompressionReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    val rows = 64

    var pixel_q = new Queue[Array[Array[Int]]]

    val poisson = scala.util.Properties.envOrElse("POISSON", "false").toBoolean
    var poissonString = ""
    if (poisson) poissonString = " (poisson)"

    def insert_pixels(c: CompressionReduction, pixels: Array[Array[Int]]) {
        val ordered = new ListBuffer[Int]
        for (i <- 0 until pixels.length by 4) {
            for (j <- 0 until 8 by 4) {
                for (k <- 0 until 4) {
                    for (l <- 0 until 4) {
                        c.io.pixels(i+k)(j+l).poke(pixels(i+k)(j+l).U)
                        ordered += pixels(i+k)(j+l)
                    }
                }
            }
        }
        //println(ordered.toList)
        pixel_q += pixels
    }

    def get_headers(data: List[Int]) : (List[Int], Int) = {
        //val l2bits = (0 until rows / 16).map(i => (0 until 8).map(j => ((ihave(i) >> (14 - 2*j)) & 3).toInt)).flatten
        println(ihave.slice(0, 8))
        val l2bits = ListBuffer.empty[Int]
        for (i <- 0 until rows / 16) {
            for (j <- 0 until 8) {
                l2bits += ((data(i) >> (14 - 2*j)) & 3).toInt
            }
        }
        println(l2bits)
        val l4bits = ListBuffer.empty[Int]
        var i4 = 0
        for (i <- 0 until l2bits.length) {
            if (l2bits(i) == 3) {
                l4bits += ((data(rows / 16 + i4/4) >> (12 - 4*(i4 % 4))) & 15).toInt
                i4 += 1
            } else {
                l4bits += l2bits(i)
            }
        }

        return (l4bits.toList, i4)
    }

    var missing = 0
    var ihave = List.empty[Int]
    def check_block(block: BigInt) {
        println("CHECKING")
        val poisson = (block >> 1022) & 1
        val pos = (block >> 1016) & 63
        val frame_num = (block >> 1008) & 255
        val data = (0 until 63).map(i => ((block >> 16*(62 - i)) & ((1 << 16) - 1)).toInt)

        var xor = 0
        for (i <- 0 until 1024) {
            xor ^= ((block >> i) & 1).toInt
        }
        assert(xor == 1, "Block did not have an odd parity")

        assert((pos == 63 && missing >= 63) || pos == missing || missing == -1, "Given shift position did not match")
        
        missing = 0
        ihave = ihave ++ data
        var first = true
        try {
            while (missing == 0 && ihave.length > 0) {
                val (l4bits, i4) = get_headers(ihave)

                println(l4bits)
                println(i4)

                val datlen = l4bits.reduce((a, b) => a + b)
                val metalen = rows/16 + (i4 + 3)/4
                val totlen = metalen + datlen
                if (ihave.length < totlen) {
                    missing = totlen - ihave.length
                } else {
                    val shuffled = Array.fill(rows * 8 / 16)(Array.fill(10)(0))

                    var idx = metalen;
                    for (i <- 0 until shuffled.length) {
                        for (j <- 0 until l4bits(i)) {
                            shuffled(i)(j) = ihave(idx)
                            idx += 1
                        }
                    }

                    val pixels = Array.ofDim[Int](rows, 8)
                    for (i <- 0 until shuffled.length) {
                        val pix = Array.fill(16)(0)
                        for (j <- 0 until 10) {
                            for (k <- 0 until 16) {
                                pix(k) <<= 1
                                pix(k) += shuffled(i)(9 - j) & (1 << k)
                            }
                        }

                        for (j <- 0 until 16) {
                            pixels(i / 2 + j / 4)(i % 2 + (j % 4)) = pix(j)
                        }
                    }

                    val ref = pixel_q.dequeue
                    for (i <- 0 until rows) {
                        for (j <- 0 until 8) {
                            assert(ref(i)(j) == pixels(i)(j), "Pixels did not match")
                        }
                    }
                    print("MATCHED")

                    ihave = ihave.slice(totlen, ihave.length)
                    /*try {
                        val (n4bits, ni4) = get_headers(ihave)
                        missing = rows / 16 + (ni4 + 3) / 4 + n4bits.reduce((a, b) => a + b) - ihave.length
                        if (missing < 0) missing = 0
                    } catch {
                        case _ : Exception => { missing = -1 }
                    }*/
                }
            }
        } catch {
            case _ : Exception => { missing = -1 }
        }
    }

    "CompressionReduction" should "work with random data" + poissonString taggedAs FullTestTag in {
        test(new CompressionReduction(rows, 8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val r = new Random(1)

            println(c.numblocks)

            val poisson = false

            for (i <- 0 until rows) {
                for (j <- 0 until 8) {
                    c.io.pixels(i)(j).poke(0.U)
                }
            }

            c.io.fifo_full.poke(0.B)
            c.io.soft_rst.poke(0.B)
            c.io.bypass_compression.poke(0.B)
            c.io.poisson.poke(poisson.B)
            c.io.data_valid.poke(0.B)
            c.io.frame_sync.poke(1.B)
            c.clock.step()
            c.io.data_valid.poke(1.B)
            c.io.frame_sync.poke(0.B)

            for (n <- 0 until 100) {
                val pixels = generate_pixels(r, rows)
                //println(pixels.map(_.mkString(" ")).mkString("\n"))
                println("Inserted")
                insert_pixels(c, pixels)

                if (c.io.write_enable.peek().litValue == 1) {
                    val numblocks = c.io.blocks_used.peek().litValue.toInt
                    println("Got " + numblocks + " blocks")

                    val blocks = new Array[BigInt](numblocks)
                    for (i <- 0 until numblocks) {
                        check_block(c.io.blocks(i).peek().litValue)
                    }
                }

                c.clock.step()
            }
        }
    }
}