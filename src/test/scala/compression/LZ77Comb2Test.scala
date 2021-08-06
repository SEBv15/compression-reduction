package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Tests LZ77Comb2 a bit
 *
 *  @author Sebastian Strempfer
 */
class LZ77Comb2Test extends FlatSpec with ChiselScalatestTester with Matchers {
    "LZ77Comb2" should "should work for some inputs" in {
        test(new LZ77Comb2(128, 16, 32)) { c =>
            for (i <- 0 until 128)
                c.io.data(i).poke((i + 1).U)
            for (i <- 0 until 128 + 32)
                c.io.compare_to(i).poke(i.U)

            var out = new ListBuffer[Int] 
            for (i <- 0 until 256)
                out += c.io.out.data(i).peek().litValue.toInt

            var encodings = new ListBuffer[(Int, Int)]
            for (i <- 0 until 128) {
                encodings = encodings :+ (c.io.encodings(i).pos.peek().litValue.toInt, c.io.encodings(i).len.peek().litValue.toInt)
            }
           
            println(c.io.out.len.peek())
            println(out.toList)
            println(encodings.toList)

            for (i <- 0 until 128)
                c.io.data(i).poke(1.U)
            for (i <- 0 until 128 + 32)
                c.io.compare_to(i).poke(0.U)

            out = new ListBuffer[Int] 
            for (i <- 0 until 256)
                out += c.io.out.data(i).peek().litValue.toInt
           
            encodings = new ListBuffer[(Int, Int)]
            for (i <- 0 until 128) {
                encodings = encodings :+ (c.io.encodings(i).pos.peek().litValue.toInt, c.io.encodings(i).len.peek().litValue.toInt)
            }
          
            println(c.io.out.len.peek())
            println(out.toList)
            println(encodings.toList)
        }
    }
}
