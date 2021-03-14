package compression

import org.scalatest._
import chiseltest._
import chisel3._

/** Basic test for the LengthCompress module. Could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class LengthCompressionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    it should "test-length-compress" in {
        // test case body here
        test(new LengthCompress) { c =>
        // test body here
            // test zeros
            for (i <- 0 until 16) {
                c.io.in(i).poke(0.U)
            }
            c.io.header.expect(0.U)
            for (i <- 0 until 10) {
                c.io.data(i).expect(0.U)
            }
            println(c.io.header.peek())

            // test ones
            for (i <- 0 until 16) {
                c.io.in(i).poke(((1 << 10) - 1).U)
            }
            c.io.header.expect(10.U)
            for (i <- 0 until 10) {
                c.io.data(i).expect(((1 << 16) - 1).U)
            }
            println(c.io.header.peek())            

            // test weird 1
            for (i <- 0 until 16) {
                c.io.in(i).poke(123.U)
            }
            c.io.header.expect(7.U)
            for (i <- 0 until 10) {
                if (i == 2 || i > 6) {
                    c.io.data(i).expect(0.U)
                } else {
                    c.io.data(i).expect(((1 << 16) - 1).U)
                }
            }
            println(c.io.header.peek())          

            // test weird 2
            for (i <- 0 until 16) {
                c.io.in(i).poke((if (i == 7) {1} else {0}).U)
            }
            c.io.header.expect(1.U)
            for (i <- 0 until 10) {
                if (i == 0) {
                    c.io.data(i).expect((1 << 7).U)
                } else {
                    c.io.data(i).expect(0.U)
                }
            }
            println(c.io.header.peek())             
        }
    }
}
