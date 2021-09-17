package compression

import org.scalatest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chisel3._
import chisel3.util.log2Floor

import scala.math.pow
import scala.math.floor
import scala.math.max

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

import scala.util.Random

import testUtils._

import java.io._

import org.scalatest.Tag

/** A simple testbench for the Coalescing module that verifies what comes in also comes out
 *
 *  Depeding on the number of inputs this can take a couple minutes to run
 *
 *  @author Sebastian Strempfer
 */
class CoalescingTest extends FlatSpec with ChiselScalatestTester with Matchers {
    val num_inputs = 16
    val input_maxsize = 10
    var processing = new Queue[Int]
    "Coalescing" should "work with random data" in {
        test(new Coalescing(num_inputs = num_inputs, input_maxsize = input_maxsize)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            // Use a seeded random value for test reproducability. You can change or remove the seed.
            val r = new Random(1)

            // Simulate 100 clock cycles
            for (n <- 0 until 100) {
                // Generate the constant-sized metadata input
                val metadata = Seq.fill(num_inputs)(r.nextInt(16))

                // Put the metadata in the queue
                for (i <- 0 until num_inputs by 4) {
                    var e = 0
                    for (j <- 0 until 4) {
                        e <<= 4
                        e += metadata(i + j)
                    }
                    processing += e
                }

                // Generate the variable length values
                for (i <- 0 until num_inputs) {
                    // Insert the metadata
                    c.io.input_metadata(i).poke(metadata(i).U)

                    // Generate a random length for the variable-length data
                    val len = r.nextInt(input_maxsize + 1)
                    // and give it to the module
                    c.io.inputs(i).len.poke(len.U)

                    for (j <- 0 until input_maxsize) {
                        // Generate random elements for the variable-length input and insert it
                        val e = r.nextInt(1 << 16)
                        c.io.inputs(i).data(j).poke(e.U)

                        // If it is within the length, also add it to the queue
                        if (j < len) processing += e
                    }
                }

                // Check if the module output matches our processing queue content
                if (c.io.write_enable.peek().litValue == 1) {
                    println("Got data!")

                    // Get the output as a BigInt
                    var output = c.io.output.peek().litValue

                    // Divide it into a list of 16-bit elements
                    val out_buf = new ListBuffer[Int]
                    for (j <- 0 until c.concat_size) {
                        out_buf += (output & ((1 << 16) - 1)).toInt
                        output >>= 16
                    }
                    val out = out_buf.toList.reverse

                    // Get the expected output as a list of 16-bit words from the queue
                    val expecting = processing.slice(0, c.concat_size)
                    processing = processing.drop(c.concat_size)

                    // Check if both match
                    for (j <- 0 until c.concat_size) {
                        assert(expecting(j) == out(j))
                    }

                    println(s"All ${c.concat_size} 16-bit elements matched!")
                }

                // Advance the clock to the next cycle
                c.clock.step()
            }
        }
    }
}