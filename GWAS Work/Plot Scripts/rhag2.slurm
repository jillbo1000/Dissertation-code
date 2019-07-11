#!/bin/bash
#SBATCH -n 6
#SBATCH -N 1
#SBATCH -t 72:00:00 # 72 hours
#SBATCH -p usu-em # partition name
#SBATCH -A usu-em # account name
#SBATCH -J RH-MA # job name

#SBATCH -o out.%j
#SBATCH -e err.%j

#SBATCH --mail-user=jflundell@gmail.com
#SBATCH --mail-type=BEGIN,FAIL,END

module use ~/MyModules
module load R/3.5.2.lua

srun --multi-prog rhag2.conf


