#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=100G
#SBATCH --time=0-00:03:00

module load StdEnv/2020 gcc/11.3.0 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

Rscript ~/scratch/kx-sea-lice/R/CC/00_power_analysis.R
