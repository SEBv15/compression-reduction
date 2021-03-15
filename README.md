# Compression & Reduction

This repository contains code for the edge compression and reduction of pixel data.

## Modules

### `BitShufflePerChannel.scala`

Bit shuffling module that groups bits by significance

### `CompressionReduction.scala`

Main module that incorporates all the other modules. Takes in pixel data, compresses it, and writes it to the FIFO.

Depends on: `EnsureBlocks.scala`, `HierarchicalReduction.scala`, `LengthCompress.scala`, `PoissonEncoding.scala`

### `CompressionReductionWrapper.scala`

Wrapper for `CompressionReduction.scala` that simplifies the ports

Depends on: `CompressionReduction.scala`

### `EnsureBlocks.scala`

This module ensures that there are always at least 2 blocks written to the FIFO. It also packs consecutive shifts to increase compression ratio. The metadata at the beginning of every block is also inserted here.

Depends on: `MakeUnusedDefault.scala`, `Merger.scala`

### `HierarchicalReduction.scala`

Takes data from the compressors, generates the hierarchical headers, and reduces the data + headers.

Depends on: `Merger.scala`, `Reduction.scala`

### `LengthCompress.scala`

The main compressor. Simply passes the data through and generates a header which indicates the position of the last non-zero word.

Depends on: `BitShufflePerChannel.scala`

### `MakeUnusedDefault.scala`

The merge modules don't respect unused data and make it whatever they like. However we want it to be all 1s. That's what this module is for.

### `Merge.scala`

Merges data by shifting the second input so the output is continuous.

### `MergeWeird.scala`

Merges data by moving elements from the end of the second input into the gap between the first and the second.

### `Merger.scala`

Module which can switch between Merge and MergeWeird (during verilog generation) based on a parameter.

Depends on: `Merge.scala`, `MergeWeird.scala`

### `PoissonEncoding.scala`

Reduces the number of bits needed to represent a pixel value by leveraging poisson noise. (lossy compression)

### `Reduction.scala`

A general reduction module which takes in a number of different vecs and outputs continuous data. This is used to reduce the headers and data. Uses `Merger.scala` internally.

Depends on: `Merger.scala`

## Usage

To generate verilog for a module
```sh
sbt 'runMain compression.[module class name]'
```

To run a test case
```sh
sbt 'testOnly compression.[test case class name] --'
