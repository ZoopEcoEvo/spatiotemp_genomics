#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

echo "Conda loaded"

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

echo "Conda env activated"

angsd -b lists/bam_list.txt -ref tonsa_genome/hic_output.fasta -out results/angsd_tonsa_exclusions/geno_lik -GL 1 -doGlf 2 -doMaf 1 -doMajorMinor 3 -nThreads 10 -sites results/angsd_tonsa_exclusions/global_snp_list.txt >& results/angsd_tonsa_exclusions/get_beagle.log

echo "Done with ANGSD"
