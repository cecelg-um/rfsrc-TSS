#!/bin/sh
export OMP_NUM_THREADS=24
srun R CMD BATCH --no-save ./hello.openmp.R