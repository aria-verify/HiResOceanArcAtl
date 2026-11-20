#!/bin/bash
#SBATCH --job-name=verify
#SBATCH --time=00:20:00
#SBATCH --account=n02-VERIFY
#SBATCH --partition=serial
#SBATCH --qos=serial

#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --cpus-per-task=1

module load globus-cli

export OMP_NUM_THREADS=1

# move data to jasmin using globus
source /work/n02/n02/ryapat30/nemo/Scripts/cli-sync.sh $cfg $case $year $month
