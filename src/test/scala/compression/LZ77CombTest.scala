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
class LZ77CombTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "LZ77Comb" should "should work for some inputs" in {
        test(new LZ77Comb(128, 8, 64)) { c =>
            for (i <- 0 until 128)
                c.io.data(i).poke((i + 1).U)
            for (i <- 0 until 128 + 64)
                c.io.compare_to(i).poke(i.U)

            var out = new ListBuffer[Int] 
            for (i <- 0 until 128)
                out += c.io.out.data(i).peek().litValue.toInt
           
            println(c.io.out.len.peek())
            println(out.toList)

            for (i <- 0 until 128)
                c.io.data(i).poke(1.U)
            for (i <- 0 until 128 + 64)
                c.io.compare_to(i).poke(0.U)

            out = new ListBuffer[Int] 
            for (i <- 0 until 128)
                out += c.io.out.data(i).peek().litValue.toInt
           
            println(c.io.out.len.peek())
            println(out.toList)
        }
    }
}
