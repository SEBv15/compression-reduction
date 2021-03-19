package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.io.Source

import scala.math.pow

import scala.util.Random

import testUtils._

/** Test the EnsureBlocks module by inputting data and checking if it comes out again in the same order
 *  
 *  @author Sebastian Strempfer
 */
class EnsureBlocksTest extends FlatSpec with ChiselScalatestTester with Matchers {
    val q = new Queue[BigInt]()

    it should "test-ensure-blocks" in {
        // test case body here
        test(new EnsureBlocks()) { c =>
            val r = new Random(1)
            val big_one: BigInt = 1

            for (n <- 0 until 100) {
                c.io.frame_num.poke(n.U)
                c.io.fifo_full.poke(0.B)
                val len = r.nextInt(16*7+5)
                c.io.len.poke(len.U)
                for (i <- 0 until len) {
                    val dat = r.nextInt((big_one << 16).toInt) // big numbers throw errors
                    q += dat
                    c.io.in(i).poke(dat.U)
                }

                if (c.io.write_enable.peek().litValue != 0) {
                    val outl = c.io.blocks_used.peek().litValue
                    var blocks = new ArrayBuffer[BigInt]()
                    for (i <- 0 until outl.toInt) {
                        blocks += c.io.out(i).peek().litValue
                    }

                    val data = blocksToData(blocks.toArray, 64)

                    for (d <- data) {
                        if (d != (big_one << 64)-1) { // filler words have all 1s set
                            assert(q.dequeue == d.toInt)
                        }
                    }
                }

                c.clock.step()
            }
        }
    }
}
