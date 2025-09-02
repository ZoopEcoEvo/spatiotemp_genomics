#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

#first generate .saf file
angsd -b lists/bam_list.txt -doSaf 1 -out results/angsd_tonsa_exclusions/sfs -anc  tonsa_genome/hic_output.fasta -GL 2 -P 100

#now try the EM optimization with 4 threads
realSFS results/angsd_tonsa_exclusions/sfs.saf.idx -maxIter 100 -P 100 >results/angsd_tonsa_exclusions/sfs_Folded.sfs
