#!/bin/bash
for i in {97..961..32}
do 
    j=$((${i} + 31))
    sbatch /home/brookson/scratch/kx-sea-lice/R/CC/run-power-analysis-glost-${i}-${j}.sh
done