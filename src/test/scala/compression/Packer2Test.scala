package compression

import org.scalatest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chisel3._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.io.Source

import scala.math.pow

import scala.util.Random

import testUtils._

/** Test the Packer2 module by inputting data and checking if it comes out again in the same order
 *
 *  Currently does not test fifo_full, but that should only affect the write_enable and I'm pretty sure it's correct.
 *
 *  @author Sebastian Strempfer
 */
class Packer2Test extends FlatSpec with ChiselScalatestTester with Matchers {
    val q = new Queue[List[Int]]()
    val poissonq = new Queue[Int]()
    val frameq = new Queue[Int]()

    val maxwords = (64*10 + 64*6/16) / 4

    "Packer" should "pack random data correctly" taggedAs UnitTestTag in {
        test(new Packer2).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            c.io.fifo_full.poke(0.B)
            c.io.poisson.poke(0.B)
            c.io.soft_rst.poke(0.B)
            c.io.may_write.poke(0.B)

            val r = new Random(1)
            var frame_num = 0
            var check_against = new Queue[Int]()
            var curr_poisson:Int = 0
            var curr_frame_num:Int = 0

            for (n <- 0 until 100) {
                if (n == 50) {
                    println("Soft Resetting")
                    c.io.may_write.poke(0.B)
                    c.io.soft_rst.poke(1.B)
                    c.clock.step()
                    c.clock.step()
                    c.io.soft_rst.poke(0.B)

                    q.clear()
                    poissonq.clear()
                    frameq.clear()
                    check_against.clear()
                }
                c.io.may_write.poke((n % 4 == 0).B)

                println("Tick " + n)
                println(q.length + " writes buffered")
                c.io.frame_num.poke(frame_num.U)

                var len = r.nextInt(maxwords + 1)
                if (n == 0) len = 0             // test zero
                if (n == 5) len = maxwords      // test full
                if (n == 6 || n == 7) len = 2   // test small

                val data = new ListBuffer[Int]()
                for (i <- 0 until len) {
                    data += r.nextInt(1 << 16)
                }

                for (i <- 0 until len) {
                    c.io.in.data(i).poke(data(i).U)
                }
                for (i <- len until maxwords) {
                    c.io.in.data(i).poke(0.U)
                }
                c.io.in.len.poke(len.U)

                // Add to queue and pad with metadata
                q += data.toList
                poissonq += 0
                frameq += frame_num

                if (c.io.write_enable.peek().litValue == 1) {
                    println("Got data on cycle", n)
                    val blocks = new Array[BigInt](16)
                    for (i <- 0 until 16) {
                        blocks(i) = c.io.out(i).peek().litValue

                        // check parity
                        val block = blocks(i)
                        var xor = 0
                        for (i <- 0 until 1024) {
                            xor ^= ((block >> i) & 1).toInt
                        }
                        assert(xor == 1, "Block did not have an odd parity")
                    }
                    val out = blocksToData(blocks, 16, headersize = 16)
                    assert(out.length == 1008, "Incorrect output length???")

                    val havechecked = Array.fill(16)(false)
                    for (i <- 0 until out.length) {
                        var prevzero = false
                        while (check_against.isEmpty) {
                            check_against = Queue(q.dequeue: _*)
                            curr_poisson = poissonq.dequeue
                            curr_frame_num = frameq.dequeue

                            // Check if the pos data is correct by checking if the first 8 bits at that location match our data
                            val block_num = i / (1008/16)
                            if (!havechecked(block_num) && !check_against.isEmpty) {
                                val pos = (blocks(block_num) >> 1016) & 63
                                println("Pos: " + pos + " in block " + block_num)
                                val posdata = (blocks(block_num) >> (1008 - pos.toInt*16 - 16)) & ((1 << 16) - 1)
                                println(check_against)
                                assert(posdata == check_against.front)
                                havechecked(block_num) = true

                                if (!prevzero) {
                                    assert(((blocks(block_num) >> 1022) & 1) == curr_poisson, "Poisson bit is incorrect")
                                    assert(((blocks(block_num) >> 1008) & 255) == curr_frame_num, "Frame num is incorrect")
                                }
                            }
                            if (check_against.isEmpty) prevzero = true
                        }
                        assert(out(i) == check_against.dequeue)
                    }
                }

                frame_num += 1
                c.clock.step()
            }
        }
    }
}