#!/bin/bash
for i in {97..965..31}
do 
    j=$((${i} + 30))
    sbatch /home/brookson/scratch/kx-sea-lice/R/CC/run-power-analysis-${i}-${j}.sh
done