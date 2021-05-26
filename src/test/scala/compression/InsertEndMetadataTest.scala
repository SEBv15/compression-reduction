package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer

/** Tests the Merge module by trying every possible combination of input lengths for a specifc configuration. The variety of configurations could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class InsertEndMetadataTest extends FlatSpec with ChiselScalatestTester with Matchers {
    it should "test insert end metada" taggedAs UnitTestTag in {
        test(new InsertEndMetadata()) { c =>
            c.io.metadata.poke(((1 << 16) - 1).U)
            for (i <- 0 until 11) {
                c.io.blocks(i).poke((1 << 16).U)
            }
            for (i <- 0 until 1024*11/64) {
                c.io.len.poke(i.U)
                for (j <- 0 until 11) {
                    if (1024*(j+1) - (i*64 + 8*(j+1)) >= 16) {
                        c.io.out(j).expect(((1 << 17) - 1).U)
                    } else {
                        c.io.out(j).expect((1 << 16).U)
                    }
                }
            }
        }
    }
}
