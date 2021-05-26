# Compression & Reduction

This repository contains chisel3 RTL code for the edge compression and reduction of pixel data. 

### High-level description

The module takes in 1024 10-bit pixels and passes them in groups of 16 to 64 individual lossless compressors. There is also an option to reduce the number of bits per pixel to 7 by leveraging the poisson noise of the pixel value (lossy). 

The compressors group the bits with same significance together and then report the position of the last non-zero element. With the assumption that most pixel values are close to zero, this method will discard a lot of zeros and yield a good compression ratio.

The variable-length output of those 64 individual compressors is then merged together to yield one continuous output.

All this happens within a single clock tick.

To most effictively use the on-chip memory, the output across multiple clock ticks is buffered and merged until a write will fill a memory block most effictively.

## Usage

The top level modules are `CompressionReduction.scala` and `CompressionReductionWrapper.scala` where the latter is simply a wrapper that makes the ports easier to use with verilog.

### Generating Verilog

To get the verilog of an arbitrary module (including submodules) use
```sh
sbt 'runMain compression.[module class name]'
```

The top level module can be generated using
```sh
sbt 'runMain compression.CompressionReductionWrapper'
```

### Testing
Most of the modules have test cases, which can be run using
```sh
sbt 'testOnly compression.[test case class name] --'
```

To run all test cases on submodules use
```sh
sbt 'testOnly compression.* -- -n "unitTest"'
```

And to run the testbench for the entire module use
```sh
sbt 'testOnly compression.* -- -n "fullTest"'
```

running tests can take a considerable amount of time on large modules. For example running the `fullTest` suite takes around 15 minutes.