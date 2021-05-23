package compression

import org.scalatest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest._
import chisel3._

import scala.collection.mutable.ListBuffer

/** Tests the Merge module by trying every possible combination of input lengths for a specifc configuration. The variety of configurations could be more comprehensive
 *
 *  @author Sebastian Strempfer
 */
class MergeVerilogTest extends FlatSpec with ChiselScalatestTester with Matchers {
    def testwith(inwords1: Int, inwords2: Int) = {
        test(new MergeVerilogWrapper(16, inwords1, inwords2)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            for (i <- 0 until inwords1) {
                c.io.data1(i).poke((i+1).U)
            }
            for (i <- 0 until inwords2) {
                c.io.data2(i).poke((i+11).U)
            }
            for (len1 <- 0 to inwords1) {
                for (len2 <- 0 to inwords2) {
                    val l1 = List.range(1, len1+1)
                    val l2 = List.range(1+10, len2+1+10)
                    val m = l1 ::: l2

                    c.io.len1.poke(len1.U)
                    c.io.len2.poke(len2.U)

                    //println(len1, len2)
                    var lo = ListBuffer[Int]()
                    for (i <- 0 until inwords1 + inwords2) {
                        lo += c.io.out(i).peek().litValue.toInt
                    }
                    //println(lo)

                    for (i <- 0 until m.length) {
                        c.io.out(i).expect(m(i).U)
                    }

                    c.io.outlen.expect((len1 + len2).U)
                }
            }
        }
    }
    it should "test MergeVerilog symmetric" taggedAs UnitTestTag in {
        testwith(10, 10)
    }
    it should "test MergeVerilog asymmetric1" taggedAs UnitTestTag in {
        testwith(10, 5)
    }
    it should "test MergeVerilog asymmetric2" taggedAs UnitTestTag in {
        testwith(32, 5)
    }
    it should "test MergeVerilog asymmetric3" taggedAs UnitTestTag in {
        testwith(5, 10)
    }
    it should "test MergeVerilog asymmetric4" taggedAs UnitTestTag in {
        testwith(5, 32)
    }
}
