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

/** Test the ShiftPacker module by inputting data and checking if it comes out again in the same order
 *  
 *  This does not test the accuracy of the frame number, soft_rst, fifo_full, or data_dropped
 *
 *  @author Sebastian Strempfer
 *  @todo Make it work again with new metadata
 */
class ShiftPackerTest extends FlatSpec with ChiselScalatestTester with Matchers {
    val q = new Queue[List[Int]]()

    val inbits = 64*(10*16 + 6)
    val wordsize = 64
    val reservebits = 8

    it should "test ShiftPacker with random data (partial)" taggedAs UnitTestTag in {
        // test case body here
        test(new ShiftPacker(inbits, wordsize, reservebits)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val r = new Random(1)
            val big_one: BigInt = 1

            c.io.soft_rst.poke(0.B)

            for (n <- 0 until 1000) {
                c.io.frame_num.poke(n.U)
                c.io.fifo_full.poke(0.B)
                val len = r.nextInt(inbits/wordsize)
                c.io.len.poke(len.U)

                val indata = ListBuffer[Int]()
                for (i <- 0 until len) {
                    val dat = r.nextInt((big_one << 30).toInt) // big numbers throw errors
                    indata += dat
                    c.io.in(i).poke(dat.U)
                }
                if (len > 0)
                    q += indata.toList

                //println("Inserted " + len + " words")

                if (c.io.write_enable.peek().litValue != 0) {
                    val outl = c.io.blocks_used.peek().litValue
                    if (outl < 2) {
                        throw new Exception("Wrote length < 2")
                    }

                    var blocks = new ArrayBuffer[BigInt]()
                    for (i <- 0 until outl.toInt) {
                        blocks += c.io.out(i).peek().litValue
                    }
                    val nmerged = (blocks(0) >> (1024-8)) & ((1 << 7) - 1)
                    //println("Got " + outl + " blocks with " + nmerged + " shifts")

                    val data = blocksToData(blocks.toArray, 64)

                    var i = 0
                    for (_ <- 0 until nmerged.toInt) {
                        val inserted = q.dequeue
                        for (word <- inserted) {
                            assert(data(i) == word)
                            i += 1
                        }
                    }
                }

                c.clock.step()
            }
        }
    }
}
