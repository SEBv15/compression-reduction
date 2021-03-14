package compression

import org.scalatest._
import chiseltest._
import chisel3._

import scala.math.pow
import scala.math.floor
import scala.math.max

import scala.collection.mutable.ListBuffer
import scala.collection.breakOut

/**
 * THIS DOES MOST LIKELY NOT WORK WITH THE CURRENT HierarchicalReduction MODULE!!!
 */

class HierarchicalReductionTest extends FlatSpec with ChiselScalatestTester with Matchers {
    // Number of compressors to test the reduction stage with (32 is max for me before running out of memory)
    val ncompressors = 64

    def weirdlyMergeLists(one:ListBuffer[Int], two:ListBuffer[Int], len1:Int, len2:Int) = {
        val totlen = one.length + two.length
        val pivot = max(0, one.length + two.length - len1)
        val twof = two ++ List.fill(len2 - two.length)(0)
        val outlist:ListBuffer[Int] = one ++ twof.slice(pivot, len1 - one.length + pivot) ++ two
        outlist.slice(0, totlen)
    }

    it should "test-hierachical-reduction" in {
        // test case body here
        test(new HierarchicalReduction(ncompressors, 7, 16, 128)) { c =>
            val r = new scala.util.Random(1)
            val lengths:ListBuffer[Int] = (0 until ncompressors).map(i => r.nextInt(10))(breakOut)
            var inp:ListBuffer[ListBuffer[Int]] = new ListBuffer[ListBuffer[Int]]
            for (i <- 0 until ncompressors) {
                inp.append(new ListBuffer[Int])
                for (j <- 0 until lengths(i)) {
                    inp(i) = inp(i) :+ r.nextInt(1024)
                }
            }

            println(inp)

            // Input the data into the reduction stage
            for (i <- 0 until inp.length) {
                for (j <- 0 until 10) {
                    c.io.datain(i)(j).poke((if (inp(i).length <= j) 0 else inp(i)(j)).U)
                }
                c.io.headerin(i).poke(lengths(i).U)
            }

            // Slowly turn the 2D list into essentially a 1D list with the data in the expected order and filler zeros where needed
            var mod = 2
            var len = 10
            var llen = 10
            while (inp.length != 1) {
                // When the reduction stage in the module merges two adjacent data, add filler zeros where needed
                if (len > 128) {
                    len /= 2
                    for (i <- 0 until inp.length) {
                        if (inp(i).length % mod != 0) {
                            val toadd = (mod - (inp(i).length % mod))
                            for (j <- 0 until toadd) {
                                inp(i) = inp(i) :+ 0
                            }
                        }
                    }
                    mod *= 2
                }
                inp = (0 until inp.length / 2).map(i => weirdlyMergeLists(inp(2*i), inp(2*i+1), llen, llen))(breakOut)
                len *= 2
                llen *= 2
            }

            // Count the number of headers > 2 and generate a list of headers > 2.
            var numheaders = 0
            var bigheaders:ListBuffer[Int] = new ListBuffer[Int]
            var bigheaders16:ListBuffer[Int] = new ListBuffer[Int]
            for (i <- 0 until lengths.length) {
                if (lengths(i) > 2) {
                    numheaders += 1
                    bigheaders = bigheaders :+ lengths(i)
                }
            }
            numheaders += (16 - numheaders % 16) // Since there might be a gap between where the headers end and the data starts, adjust the "numheaders" accordingly

            for (i <- 0 until (bigheaders.length / 4.0).ceil.toInt) {
                bigheaders16 = bigheaders16 :+ ((bigheaders.applyOrElse(4*i, (x:Int) => 0) << 12) + (bigheaders.applyOrElse(4*i+1, (x:Int) => 0) << 8) + (bigheaders.applyOrElse(4*i+2, (x:Int) => 0) << 4) + (bigheaders.applyOrElse(4*i+3, (x:Int) => 0)))
            }

            println(bigheaders.length)
            println(bigheaders16.length)
            println(bigheaders16)

            // Print the output of the reduction stage
            var out: ListBuffer[BigInt] = new ListBuffer[BigInt]
            for (i <- 0 until ncompressors * 10 + (ncompressors/8 + numheaders/4).ceil.toInt) {
                out = out :+ c.io.out(i).peek().litValue()
            }
            println(out)

            
            // Check if the 2-bit headers are correct
            val twobit_headers = (0 until ncompressors).map(x => if (lengths(x) > 2) 3 else lengths(x))
            for (i <- 0 until ncompressors/8) {
                c.io.out(i).expect(((twobit_headers(8*i) << 14) + (twobit_headers(8*i+1) << 12) + (twobit_headers(8*i+2) << 10) + (twobit_headers(8*i+3) << 8) + (twobit_headers(8*i+4) << 6) + (twobit_headers(8*i+5) << 4) + (twobit_headers(8*i+6) << 2) + (twobit_headers(8*i+7) << 0)).U)
            }

            var outdat = weirdlyMergeLists(bigheaders16, inp(0), ncompressors/4, 10*64)
            println(outdat)

            for (i <- 0 until outdat.length) {
                c.io.out(i + (ncompressors/8).ceil.toInt).expect(outdat(i).U)
            }
        }
    }
}
