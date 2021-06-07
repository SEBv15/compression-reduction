package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer

/** Tests the Merge module by trying every possible combination of input lengths for a specifc configuration. The variety of configurations could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class MergeStagedTest extends FlatSpec with ChiselScalatestTester with Matchers {
    def testwith(inwords1: Int, inwords2: Int, minwords1: Int, granularity: Int = 1) = {
        test(new MergeStaged(16, inwords1, inwords2, minwords1, 0, granularity = granularity)) { c =>
            for (i <- 0 until inwords1) {
                c.io.data1(i).poke((i+1).U)
            }
            for (i <- 0 until inwords2) {
                c.io.data2(i).poke((i+11).U)
            }
            for (len1 <- minwords1 to inwords1) {
                for (len2 <- 0 to inwords2) {
                    val padding1 = (granularity - (len1 % granularity)) % granularity
                    val padding2 = (granularity - (len2 % granularity)) % granularity
                    val l1 = List.range(1, len1+1)
                    val l2 = List.range(1+10, len2+1+10)
                    val m = l1 ::: List.fill(padding1)(0) ::: l2

                    c.io.len1.poke(len1.U)
                    c.io.len2.poke(len2.U)

                    for (i <- 0 until m.length) {
                        c.io.out(i).expect(m(i).U)
                    }

                    c.io.outlen.expect((len1 + len2 + padding1 + padding2).U)
                }
            }
        }
    }
    it should "test MergeStaged symmetric" taggedAs UnitTestTag in {
        testwith(10, 10, 0)
    }
    it should "test MergeStaged asymmetric1" taggedAs UnitTestTag in {
        testwith(10, 5, 0)
    }
    it should "test MergeStaged asymmetric2" taggedAs UnitTestTag in {
        testwith(32, 5, 0)
    }
    it should "test MergeStaged asymmetric3" taggedAs UnitTestTag in {
        testwith(5, 10, 0)
    }
    it should "test MergeStaged asymmetric4" taggedAs UnitTestTag in {
        testwith(5, 32, 0)
    }
    it should "test MergeStaged symmetric-minwords" taggedAs UnitTestTag in {
        testwith(10, 10, 5)
    }
    it should "test MergeStaged symmetric granularity" taggedAs UnitTestTag in {
        testwith(10, 10, 0, granularity = 2)
    }
}
