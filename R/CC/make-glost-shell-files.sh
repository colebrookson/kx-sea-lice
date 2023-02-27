#!/bin/bash
for i in {1000..1032..32}
do 
    echo "
    #!/bin/bash

    #SBATCH --mail-user=cole.brookson@gmail.com
    #SBATCH --mail-type=BEGIN 
    #SBATCH --mail-type=END 
    #SBATCH --mail-type=FAIL 
    #SBATCH --mail-type=REQUEUE 
    #SBATCH --job-name="power sims ${i} to ${i + 32}"
    #SBATCH --nodes=1
    #SBATCH --ntasks-per-node=32
    #SBATCH --mem-per-cpu=2GB
    #SBATCH --time=0-08:30:00

    module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3 glost/0.3.1 r/4.2.1

    export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

    srun glost_launch /home/brookson/scratch/kx-sea-lice/R/CC/power-analysis-tasks-${i}-${i+32}.txt
    
    " >> ~/Github/kx-sea-lice/R/CC/run-power-analysis-glost-${i}-${i+32}.txt 
done 
