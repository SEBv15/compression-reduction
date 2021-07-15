package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Tests FindDataLength
 *
 *  @author Sebastian Strempfer
 */
class FindDataLengthTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "FindDataLength" should "work correctly with any input" taggedAs UnitTestTag in {
        test(new FindDataLength(nelems = 10, elemsize = 1)) { c =>
            for (len <- 0 to 10) {
                for (i <- len until 10) {
                    c.io.in(i).poke(0.U)
                }
                if (len > 1) {
                    c.io.in(len - 1).poke(1.U)
                    for (data <- 0 until 1 << len - 2) {
                        for (i <- 0 until len - 1) {
                            c.io.in(i).poke((data & 1 << i).U)
                        }

                        c.io.len.peek().litValue shouldEqual len
                    }
                } else {
                    if (len == 1) c.io.in(0).poke(1.U)

                    c.io.len.peek().litValue shouldEqual len
                }
            }
        }
    }
}