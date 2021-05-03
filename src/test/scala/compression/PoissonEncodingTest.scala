package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.math.abs
import scala.math.sqrt

import testUtils._

/** Test the poisson encoding by checking if the decoded output is within half of the poisson noise of the input
 *
 *  @author Sebastian Strempfer
 */
class PoissonEncodingTest extends FlatSpec with ChiselScalatestTester with Matchers {
    it should "test-poisson-encoding" taggedAs UnitTestTag in {
        // test case body here
        test(new PoissonEncoding()) { c =>
            for (i <- 0 until 1 << 10) {
                c.io.in.poke(i.U)

                val out = c.io.out.peek().litValue.toInt

                // decode module output
                val dec = poissonDecode(out)
                //println(i, dec)

                // check if the decoded value is within half the poisson noise of the input
                assert(abs(i - dec) <= sqrt(i)/2)
            }
        }
    }
}