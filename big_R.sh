#!/bin/bash

# # Resources on test system: 20 nodes, each with 12 cores. 70GB RAM

#SBATCH --time=90:00:00 # request time (walltime, not compute time)
#SBATCH --mem=68GB # request memory. This is set up for a main job, so get as much as possible.
#SBATCH --nodes=1 # number of nodes. Need > 1 to test utilisation
#SBATCH --ntasks-per-node=10 # Cores per node # I seem to currently be limited to 10 cores.

#SBATCH -o %x_%A_%a.out # Standard output
#SBATCH -e %x_%A_%a.err # Standard error

# timing
begin=`date +%s`

module load R/4.3

Rscript $*


end=`date +%s`
elapsed=`expr $end - $begin`

echo Time taken for code: $elapsed

