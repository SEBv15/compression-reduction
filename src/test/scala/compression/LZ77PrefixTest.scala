package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Tests LZ77Prefix
 *
 *  @author Sebastian Strempfer
 */
class LZ77PrefixTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "LZ77Prefix" should "should indentify the correct prefixes" in {
        test(new LZ77Prefix(8, 4, 5)) { c =>
            for (i <- 0 until 8)
                c.io.win(i).poke(i.U)
            for (i <- 0 until 4)
                c.io.input(i).poke((i + 7).U)

            println(c.io.pos.peek())
            println(c.io.len.peek())
            println(c.io.found.peek())
        }
    }
}
