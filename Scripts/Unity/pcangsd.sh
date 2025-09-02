#!/bin/bash
#SBATCH -c 100
#SBATCH --mem=100000
#SBATCH -t 10:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

pcangsd -b results/angsd_tonsa_exclusions/geno_lik.beagle.gz -o results/pcangsd/tonsa_exclusions -t 100
