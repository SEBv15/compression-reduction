package compression

import chisel3.util.log2Floor

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import scala.math.max
import scala.math.min

object testUtils {
    /** Reverse poisson encoding as much as that is possible. 
     *  To get the returned value as close as possible to the original, add half the divisor to the output so its in center of the range of values that would lead to it.
     *  
     *  @author Sebastian Strempfer
     */
    def poissonEncode(pixel: Int) = {
        val low = pixel
        val midlow = (pixel >> 2) + 12
        val midhigh = (pixel >> 3) + 20
        val high = (pixel >> 4) + 36
        if (pixel < 16) low
        else if (pixel < 64) midlow
        else if (pixel < 256) midhigh
        else high
    }

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

    /** Reverse the operation done by a MergeWeird module
     *
     *  @author Sebastian Strempfer
     *
     *  @param data The data like it is outputted from the module
     *  @param len1 The length of the first input into the merge module
     *  @param len2 The length of the second input into the merge module
     *  @param size1 The size of the Vec holding the data for the first input (aka max len1)
     */
    def reverseMergeWeird(data: Array[BigInt], len1: Int, len2: Int, size1: Int) = {
        val pivot = if (len1 + len2 > size1) len1 + len2 - size1 else 0
        data.slice(0, len1) ++ Array.fill(size1-len1)(0) ++ data.slice(size1, size1+pivot) ++ data.slice(len1, size1)
    }

    /** Reverse the operation done by a single weird reduction stage
     *
     *  @author Sebastian Strempfer
     *
     *  @param data The data like it is outputted from the reduction stage
     *  @param lengths The lengths of the data inputted into the reduction stage
     *  @param maxblocks The maximum number of input elements into any merge module in the reduction
     *  @param elems Size of the input Vecs
     */
    def reverseWeirdReduction(data: Array[BigInt], lengths: Array[Int], maxblocks: Int = 128, elems: Int = 7): Array[BigInt] = {
        val n = lengths.length // Number of inputs into the reduction stage

        // Construct a list of the input lengths of every merge module at every stage of the reduction
        var stage_lengths = (0 until log2Floor(n)+1).map(i => ListBuffer.fill(1 << log2Floor(n) - i)(0))
        for (i <- 0 until n) {
            stage_lengths(0)(i) = lengths(i)
        }
        var l = elems
        var m = 1
        for (i <- 1 until stage_lengths.length) {
            l *= 2
            if (l > maxblocks) {
                m *= 2
                l /= 2
            }
            for (j <- 0 until stage_lengths(i).length) {
                stage_lengths(i)(j) = stage_lengths(i-1)(2*j) + stage_lengths(i-1)(2*j+1)
                if (stage_lengths(i)(j) % m != 0) {
                    stage_lengths(i)(j) += m - (stage_lengths(i)(j) % m)
                }
            }
        }

        // Create an array with room for the unreduced data
        var out: Array[BigInt] = Array.fill(n*elems)(0)
        for (i <- 0 until min(n*elems, data.length)) {
            out(i) = data(i)
        } 

        var stage = log2Floor(n)-1 // Stage number of the last stage (0-indexed)
        var inelems = n*elems/2 // Number of inputs into the last stage
        var mergers = 1 // Number of mergers in the last stage

        // Go through the stages backwards and reverse their operations
        while (stage >= 0) {
            for (m <- 0 until mergers) {
                val pivot = max(0, stage_lengths(stage)(2*m+1) - (inelems - stage_lengths(stage)(2*m)))
                val offset = inelems * 2 * m
                for (i <- 0 until stage_lengths(stage)(2*m+1) - pivot) {
                    out(offset + inelems + pivot + i) = out(offset + stage_lengths(stage)(2*m) + i)
                }
            }
            mergers *= 2
            inelems /= 2
            stage -= 1
        }

        return out
    }
}