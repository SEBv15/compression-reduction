package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.io.Source

import scala.math.pow

/** Test the EnsureBlocks module. This test is not very complete and will always pass.
 *  
 *  @author Sebastian Strempfer
 *  @todo Make better
 */
class EnsureBlocksTestOld extends FlatSpec with ChiselScalatestTester with Matchers {
    def printdata(c: EnsureBlocks, inwords: Int, wordsize: Int) {
        println("This tick we got:")
        if (c.io.write_enable.peek().litValue != 0) {
            val len = c.io.blocks_used.peek().litValue
            println(len)
            var offs = wordsize - 8
            for (i <- 0 until len.toInt) {
                var value = c.io.out(i).peek().litValue
                println(value.toString(2))
                val mask = (pow(2, wordsize)-1).toLong
                var l = new ListBuffer[BigInt]()
                println("Is first frame", value >> 1023)
                println("Frame Num", (value >> 1016) & ((1 << 7) - 1))
                println((value >> 1016 - 64) & mask)
                for (j <- 0 until 1024/wordsize) {
                    l.append(value & mask)
                    if (j == 0) {
                        value >>= offs
                    } else {
                        value >>= wordsize 
                    }
                }
                offs = (offs + wordsize - 8) % wordsize
                println(l)
            }
        }
    }
    it should "test-ensure-blocks" in {
        // test case body here
        test(new EnsureBlocks()) { c =>
            val inwords = (64*7*16 + 64*5)/64
            val wordsize = 64
            c.io.frame_num.poke(1.U)
            for (i <- 0 until inwords) {
                c.io.in(i).poke(13.U)
            }
            c.io.len.poke(inwords.U)
            c.io.fifo_full.poke(0.B)
            printdata(c, inwords, wordsize)
            c.clock.step()

            c.io.frame_num.poke(2.U)
            for (i <- 0 until inwords) {
                c.io.in(i).poke(12.U)
            }    
            c.io.len.poke(1.U)    
            printdata(c, inwords, wordsize)
            c.clock.step()

            c.io.frame_num.poke(3.U)
            for (i <- 0 until inwords) {
                c.io.in(i).poke(11.U)
            }    
            c.io.len.poke(5.U)
            printdata(c, inwords, wordsize)
            c.clock.step()

            c.io.frame_num.poke(4.U)
            for (i <- 0 until inwords) {
                c.io.in(i).poke(10.U)
            }    

            c.io.len.poke(inwords.U)
            printdata(c, inwords, wordsize)
            c.clock.step()

            printdata(c, inwords, wordsize)
        }
    }
}
