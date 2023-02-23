#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=5
#SBATCH --mem-per-cpu=4G
#SBATCH --time=0-00:10:00

module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3 glost/0.3.1 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

srun glost_launch /home/brookson/scratch/kx-sea-lice/R/CC/power-analysis-tasks.txt
