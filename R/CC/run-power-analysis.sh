#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=2G
#SBATCH --time=0-00:01:00

module load gcc/11.3.0 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

parallel -j $SLURM_CPUS_PER_TASK 'Rscript ~/scratch/kx-sea-lice/R/CC/00_power_analysis.R {}' ::: {1..4}
