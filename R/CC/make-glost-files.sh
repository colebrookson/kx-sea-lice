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
        if [ "${j}" -lt "$var" ] # this is because the last line can't be empty for the GLOST to work properly apparently
        then
            envname='${nargument}'
            echo "nargument=${j} && Rscript /home/brookson/scratch/kx-sea-lice/R/CC/00_power_analysis.R ${envname}" >> ~/Github/kx-sea-lice/R/CC/power-analysis-tasks-${i}-${var}.txt
        fi
        if [ "${j}" -eq "$var" ]
        then
            envname='${nargument}'
            echo -n "nargument=${j} && Rscript /home/brookson/scratch/kx-sea-lice/R/CC/00_power_analysis.R ${envname}" >> ~/Github/kx-sea-lice/R/CC/power-analysis-tasks-${i}-${var}.txt 
        fi
    done
done
