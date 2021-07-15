package compression

import org.scalatest._
import chiseltest._
import chisel3._

/** Basic test for the DynamicBitWidthCompress module. Could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class DynamicBitWidthCompressTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "DynamicBitWidthCompress" should "work with all zero input" taggedAs UnitTestTag in {
        test(new DynamicBitWidthCompress) { c =>
            for (i <- 0 until 16) {
                c.io.in(i).poke(0.U)
            }
            c.io.out.len.expect(0.U)
            for (i <- 0 until 10) {
                c.io.out.data(i).expect(0.U)
            }
        }
    }

    it should "work with all ones input" taggedAs UnitTestTag in {
        test(new DynamicBitWidthCompress) { c =>
            for (i <- 0 until 16) {
                c.io.in(i).poke(((1 << 10) - 1).U)
            }
            c.io.out.len.expect(10.U)
            for (i <- 0 until 10) {
                c.io.out.data(i).expect(((1 << 16) - 1).U)
            }
        }
    }

    it should "work with some weird input" taggedAs UnitTestTag in {
        test(new DynamicBitWidthCompress) { c =>
            // test 1
            for (i <- 0 until 16) {
                c.io.in(i).poke(123.U)
            }
            c.io.out.len.expect(7.U)
            for (i <- 0 until 10) {
                if (i == 2 || i > 6) {
                    c.io.out.data(i).expect(0.U)
                } else {
                    c.io.out.data(i).expect(((1 << 16) - 1).U)
                }
            }

            // test 2
            for (i <- 0 until 16) {
                c.io.in(i).poke((if (i == 7) {1} else {0}).U)
            }
            c.io.out.len.expect(1.U)
            for (i <- 0 until 10) {
                if (i == 0) {
                    c.io.out.data(i).expect((1 << 7).U)
                } else {
                    c.io.out.data(i).expect(0.U)
                }
            }
        }
    }       
}
