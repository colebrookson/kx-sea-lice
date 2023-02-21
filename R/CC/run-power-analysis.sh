#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=4G
#SBATCH --time=0-00:06:00

module load gcc/11.3.0 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

<<<<<<< HEAD
parallel -j $SLURM_CPUS_PER_TASK 'Rscript ~/scratch/kx-sea-lice/R/CC/00_power_analysis.R {}' ::: {1..2}
=======
parallel -j $SLURM_CPU_PER_TASK 'Rscript ~/scratch/kx-sea-lice/R/CC/00_power_analysis.R' 
>>>>>>> 61203d42b3e1a34b7474bb814495d3deeeacb2d0
