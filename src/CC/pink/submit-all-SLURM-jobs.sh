#!/bin/bash
for i in {1..993..31}
do 
    j=$((${i} + 30))
    sbatch /home/brookson/scratch/kx-sea-lice/R/CC/pink/run-power-analysis-${i}-${j}.sh
done
sbatch /home/brookson/scratch/kx-sea-lice/R/CC/pink/run-power-analysis-993-1000.sh