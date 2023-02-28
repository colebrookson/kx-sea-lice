#!/bin/bash
for i in {97..961..32}
do 
    j=$((${i} + 31))
    var='$EBVERSIONR'
    echo "#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --job-name='power sims ${i} to ${j}'
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --mem-per-cpu=3GB
#SBATCH --time=0-06:30:00

module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3 glost/0.3.1 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$var/

srun glost_launch /home/brookson/scratch/kx-sea-lice/R/CC/power-analysis-tasks-${i}-${j}.txt" >> ~/Github/kx-sea-lice/R/CC/run-power-analysis-glost-${i}-${j}.sh 
done 
