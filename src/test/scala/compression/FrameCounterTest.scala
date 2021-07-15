package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Tests the FrameCounter
 *
 *  @author Sebastian Strempfer
 */
class FrameCounterTest extends FlatSpec with ChiselScalatestTester with Matchers {
    "FrameCounter" should "work correctly" taggedAs UnitTestTag in {
        test(new FrameCounter(16)) { c =>
            c.io.data_valid.poke(1.B)
            c.io.soft_rst.poke(0.B)
            c.io.frame_sync.poke(0.B)

            // check if it stays zero until it receives first sync
            for (i <- 0 until 100) {
                c.clock.step()
                c.io.shift_num.expect(0.U)
                c.io.received_first_sync.expect(0.B)
            }

            // Check normal operation
            var counter = 16
            for (i <- 0 until 10) {
                c.io.frame_sync.poke(1.B)
                for (j <- 0 until 16) {
                    c.clock.step()
                    c.io.shift_num.expect(counter.U)
                    counter += 1
                    c.io.received_first_sync.expect(1.B)

                    c.io.frame_sync.poke(0.B)
                }
            }

            // soft reset
            c.io.soft_rst.poke(1.B)
            c.clock.step()
            c.io.soft_rst.poke(0.B)
            c.io.shift_num.expect(0.U)
            c.io.received_first_sync.expect(0.B)

            // check skipping every 2nd frame
            counter = 16
            for (i <- 0 until 10) {
                c.io.frame_sync.poke(1.B)
                for (j <- 0 until 16) {
                    c.clock.step()
                    c.io.frame_sync.poke(0.B)

                    c.io.shift_num.expect(counter.U)
                    c.io.received_first_sync.expect(1.B)

                    c.io.data_valid.poke(0.B)
                    c.clock.step()
                    c.io.shift_num.expect(counter.U)
                    c.io.received_first_sync.expect(1.B)
                    c.io.data_valid.poke(1.B)

                    counter += 1
                }
            }

            // soft reset
            c.io.soft_rst.poke(1.B)
            c.clock.step()
            c.io.soft_rst.poke(0.B)
            c.io.shift_num.expect(0.U)
            c.io.received_first_sync.expect(0.B)

            // check randomly setting frame_sync high
            counter = 0
            val r = new Random()
            for (i <- 0 until 100) {
                c.io.frame_sync.poke(1.B)
                counter = (((counter >> 4) + 1) << 4)
                for (j <- 0 until r.nextInt(15)+1) {
                    c.clock.step()
                    c.io.shift_num.expect(counter.U)
                    counter += 1
                    c.io.received_first_sync.expect(1.B)

                    c.io.frame_sync.poke(0.B)
                }
            }
        }
    }
}
