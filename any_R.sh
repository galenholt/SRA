#!/bin/bash

# # Resources on test system: 20 nodes, each with 12 cores. 70GB RAM

#SBATCH --time=14:00:00 # request time (walltime, not compute time)
#SBATCH --mem=500MB # request memory. This is just a coordinator, so shouldn't need its own memory
#SBATCH --nodes=1 # number of nodes. Need > 1 to test utilisation
#SBATCH --ntasks-per-node=1 # Cores per node

#SBATCH -o %x_%A_%a.out # Standard output
#SBATCH -e %x_%A_%a.err # Standard error

# timing
begin=`date +%s`

module load R/4.3

Rscript $*


end=`date +%s`
elapsed=`expr $end - $begin`

echo Time taken for code: $elapsed

