#!/bin/bash
for i in {65..961..32}
do 
    j=$((${i} + 31))
    sbatch /home/brookson/scratch/kx-sea-lice/R/CC/run-power-analysis-${i}-${j}.sh
done