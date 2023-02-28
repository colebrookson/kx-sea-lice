#!/bin/bash
for i in {97..993..32}
do
    if [ "${i}" -lt "993" ] 
    then
        var=$(($i + 31))
    fi
    if [ "${i}" -eq "993" ]
    then 
        var=$(($i + 7))
    fi
    for ((j=$i; j<=$var; j++))
    do
        envname='${nargument}'
        echo "nargument=${j} && Rscript /home/brookson/scratch/kx-sea-lice/R/CC/00_power_analysis.R ${envname}" >> ~/Github/kx-sea-lice/R/CC/power-analysis-tasks-${i}-${var}.txt 
    done
done