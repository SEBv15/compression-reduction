package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Tests PatternEncoder
 *
 *  @author Sebastian Strempfer
 */
class PatternEncoderTest extends FlatSpec with ChiselScalatestTester with Matchers {
    def tryInput(c: PatternEncoder, binaryString: String, shouldencode: Boolean, hi: Int, lo: Int, isrun: Boolean) = {
        c.io.in.poke(Integer.parseInt(binaryString.replaceAll(" ", ""), 2).U)

        //println(binaryString)
        //println(c.io.out.peek().litValue.toInt.toBinaryString)
        //println(c.io.canencode.peek())
        assert(shouldencode == (c.io.canencode.peek().litValue.toInt == 1))
        val out = c.io.out.peek().litValue.toInt
        val run = (out >> 7) == 1
        val h = (out >> 4) & 7
        val l = out & 15
        if (shouldencode) {
            if (hi == -1) {
                assert(h + l == 15)
                assert(out >> 4 == lo)
            } else if (hi < 8) {
                assert(h + l < 15)
                assert(h == hi)
                assert(l == lo)
                assert(run == isrun)
            } else {
                assert(h + l > 15)
                assert(15 - h == hi)
                assert(15 - l == lo)
                assert(run == isrun)                
            }
        }
    }

    "PatternEncoder" should "encode correctly" taggedAs UnitTestTag in {
        test(new PatternEncoder) { c =>
            //          Input            can encode |hi |lo |is run?
            tryInput(c, "0000 0000 0000 0000", false, 0 , 0 , false)
            tryInput(c, "0001 1111 0000 0000", true , 3 , 8 , true )
            tryInput(c, "1000 0000 0000 0000", true , -1, 15, false)
            tryInput(c, "0000 0000 0000 0001", true , -1, 0 , false)
            tryInput(c, "0000 0000 0100 0001", true , 9 , 0 , false)
            tryInput(c, "0000 0000 1100 0001", false, 0 , 0 , false)
            tryInput(c, "0000 0000 1100 0000", true , 8 , 6 , true )
            tryInput(c, "1111 1111 1111 1111", true , 0 , 0 , true )
            tryInput(c, "1000 0000 0000 0001", true , 0 , 0 , false)
        }
    }
}
