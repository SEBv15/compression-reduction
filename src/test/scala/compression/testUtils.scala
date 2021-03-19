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

    /** Concatenate an array like chisel would if it were a Vec
     *
     *  @author Sebastian Strempfer
     * 
     *  @param data The array to concatenate
     *  @param wordsize Number of bits in each array element
     */
    def intListConcat(data: Array[BigInt], wordsize: Int) = {
        var out: BigInt = 0
        for (e <- data) {
            out <<= wordsize
            out += e
        }
        out
    }

    /** Reverse the operation done by a MergeWeird module
     *
     *  @author Sebastian Strempfer
     *
     *  @param data The data like it is outputted from the module
     *  @param len1 The length of the first input into the merge module
     *  @param len2 The length of the second input into the merge module
     *  @param size1 The size of the Vec holding the data for the first input (aka max len1)
     *
     *  @return The input into the merge module
     */
    def reverseMergeWeird(data: Array[BigInt], len1: Int, len2: Int, size1: Int): Array[BigInt] = {
        val pivot = if (len1 + len2 > size1) len1 + len2 - size1 else 0
        val padding: Array[BigInt] = Array.fill(size1-len1)(0)
        data.slice(0, len1) ++ padding ++ data.slice(size1, size1+pivot) ++ data.slice(len1, size1)
    }

    /** Calculates how long the output of a reduction module would be based on the input lengths
     *
     *  @author Sebastian Strempfer
     */
    def calculateReductionOutputLength(lengths: Array[Int], maxblocks: Int = 128, elems: Int = 7): Int = {
        val ls = new Array[Int](lengths.length)
        lengths.copyToArray(ls)

        var ms = ls.length / 2
        var m = 1
        var l = elems
        while (ms >= 1) {
            l *= 2
            if (l > maxblocks) {
                l /= 2
                m *= 2
            }
            for (i <- 0 until ms) {
                ls(i) = ls(2*i) + ls(2*i+1)
                ls(i) += (m - (ls(i) % m)) % m
            }

            ms /= 2
        }
        return ls(0)
    }

    /** Reverse the operation done by a single weird reduction stage
     *
     *  @author Sebastian Strempfer
     *
     *  @param data The data like it is outputted from the reduction stage
     *  @param lengths The lengths of the data inputted into the reduction stage
     *  @param maxblocks The maximum number of input elements into any merge module in the reduction
     *  @param elems Size of the input Vecs
     *
     *  @return Reduction stage input data as a flattened 2D array
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

    /** Get headers from HierarchicalReduction output
     *
     *  @author Sebastian Strempfer
     *
     *  @param data The header data like it is outputted from the HierarchicalReduction stage
     *  @param ncompressors The number of compressors the headers come from
     *  @param headersize Number of bits in the full header
     *  @param elemwidth The bit width of each element in the input
     *
     *  @return Tuple of the headers and the number of full-sized headers
     */
    def getHeaders(data: Array[BigInt], ncompressors: Int = 64, headersize: Int = 3, elemwidth: Int = 16) = {
        val num2bitelems = (ncompressors*2 + elemwidth)/elemwidth
        var twobit_data = intListConcat(data.slice(0, num2bitelems), elemwidth)
        twobit_data >>= elemwidth - (ncompressors*2 % elemwidth)
        val twobit_headers = (0 until ncompressors*2 by 2).map(i => (twobit_data >> (ncompressors - 1)*2 - i) & 3)

        var threebit_data = intListConcat(data.slice(ncompressors*2/elemwidth, (ncompressors*(2 + headersize) + elemwidth-1)/elemwidth), elemwidth)
        threebit_data >>= (elemwidth - (ncompressors*(2 + headersize) % elemwidth)) % elemwidth
        val threebit_headers = (0 until ncompressors*headersize by headersize).map(i => (threebit_data >> (headersize*(ncompressors-1)) - i) & ((1 << headersize)-1))

        var i3 = 0
        var headers = new Array[Int](ncompressors)
        for (i <- 0 until ncompressors) {
            if (twobit_headers(i) < 2) {
                headers(i) = twobit_headers(i).toInt
            } else {
                headers(i) = threebit_headers(i3).toInt
                i3 += 1
            }
        }
        (headers, i3)
    }
}