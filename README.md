# GEM - Software for kinetic simulation of magnetically confined fusion plasmas
The Direct directory contains source files for the split-weight control-variate mathod

The Hybrid directory contains source files for the gyrokinetic ion/fluid electron hybrid model


## Changes to code in Direct (Matt Mitchell)

### Build system

The code now compiles in subdirectories. The main advantage of this is that we can have test
multiple versions of the executable without having to rebuild each time or edit the Makefile (for
example, Haswell and KNL nodes may require different compilation flags for the best performance). If
we want to test a new set of build options, we can just add it to the Makefile. See the Makefile for
a full list of existing compilation targets.

To build, you should execute `make build`, `make build-knl`, `make vtune`, or something similar
depending on the target. You'll need to copy the input file to the target dpirectory.

For the VTune build targets, the build directory should be a symlink to a filesystem in NERSC's
project directory. This is because memory mapping isn't supported in the filesystem for home
directories.

### Profiling

There's a lot of flags throughout the code for profiling (like `profjie` for the jie
subroutine). Setting these to .True. will report the run times for various parts of the code.

### Regression testing

`regtest.f90` contains a module for regression testing. This allows us to optimize the code and make
sure our optimizations don't produce any errors. To start, run the code for some reference case
(this is determined by the first parameter to functions like `regtest_jpar0`). It will save the
input variables and all relevant global arrays in the project directory. I've already done this for
`jie` and `jpar0`. Then, you can use the `verify_` flags at the top of gem.f90 to make sure the code
produces identical output across every single MPI task.

If you'd like to write your own regression test, you can use the existing functions in `regtest.f90`
as a template. `varparse.py` should help determine the relevant global variables in each
function, but be warned, it produces a lot of false positives.

### Actual optimizations

Most of the optimization work is done in `jie` and `jpar0`, and involves strip-mining loops. For
most of the loops, the deposition is the costly part because it can't be done in parallel or
vectorized, as multiple threads could write to the same location. The modified code saves the
weights and deposition locations for each particle in arrays, and iterates over them in a new loop
(each thread might iterate over a different part of the array).
