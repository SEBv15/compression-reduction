package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer

/** Tests the Merge module by trying every possible combination of input lengths for a specifc configuration. The variety of configurations could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class MergeTest extends FlatSpec with ChiselScalatestTester with Matchers {
    def testwith(inwords1: Int, inwords2: Int, minwords1: Int, granularity: Int = 1) = {
        test(new Merge(16, inwords1, inwords2, minwords1, 0, granularity = granularity)) { c =>
            for (i <- 0 until inwords1) {
                c.io.in1.data(i).poke((i+1).U)
            }
            for (i <- 0 until inwords2) {
                c.io.in2.data(i).poke((i+11).U)
            }
            for (len1 <- minwords1 to inwords1) {
                for (len2 <- 0 to inwords2) {
                    val padding1 = (granularity - (len1 % granularity)) % granularity
                    val padding2 = (granularity - (len2 % granularity)) % granularity
                    val l1 = List.range(1, len1+1)
                    val l2 = List.range(1+10, len2+1+10)
                    val m = l1 ::: List.fill(padding1)(0) ::: l2

                    c.io.in1.len.poke(len1.U)
                    c.io.in2.len.poke(len2.U)

                    for (i <- 0 until m.length) {
                        c.io.out.data(i).expect(m(i).U)
                    }

                    c.io.out.len.expect((len1 + len2 + padding1 + padding2).U)
                }
            }
        }
    }
    "Merge" should "work with symmetric max input lengths" taggedAs UnitTestTag in {
        testwith(10, 10, 0)
    }
    it should "work with larger in1" taggedAs UnitTestTag in {
        testwith(10, 5, 0)
    }
    it should "work with much larger in1" taggedAs UnitTestTag in {
        testwith(32, 5, 0)
    }
    it should "work with larger in2" taggedAs UnitTestTag in {
        testwith(5, 10, 0)
    }
    it should "work with much larger in2" taggedAs UnitTestTag in {
        testwith(5, 32, 0)
    }
    it should "handle minwords" taggedAs UnitTestTag in {
        testwith(10, 10, 5)
    }
    it should "work with different granularity" taggedAs UnitTestTag in {
        testwith(10, 10, 0, granularity = 2)
    }
}
