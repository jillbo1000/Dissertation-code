#!/bin/bash
#SBATCH -n 32
#SBATCH -N 1
#SBATCH -t 72:00:00 # 72 hours
#SBATCH -p usu-em # partition name
#SBATCH -A usu-em # account name
#SBATCH -J GBM-OP1 # job name

#SBATCH -o out.%j
#SBATCH -e err.%j

#SBATCH --mail-user=jflundell@gmail.com
#SBATCH --mail-type=BEGIN,FAIL,END

module use ~/MyModules
module load R/3.4.4.lua

srun --multi-prog opt1.conf


