package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.math.abs
import scala.math.sqrt

/** Test the poisson encoding by checking if the decoded output is within half of the poisson noise of the input
 *
 *  @author Sebastian Strempfer
 */
class PoissonEncodingTest extends FlatSpec with ChiselScalatestTester with Matchers {
    // Decode by reversing the operations done by the encoding module and also adding half the divisor to get the number closer to the original
    def poissonDecode(enc:Int): Int = {
        if (enc < 16) 
            return enc
        if (enc < 28)
            return ((enc-12) << 2) + 2
        if (enc < 52)
            return ((enc-20) << 3) + 4
        return ((enc-36) << 4) + 8
    }

    it should "test-poisson-encoding" in {
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