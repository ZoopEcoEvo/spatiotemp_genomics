#!/bin/bash
#SBATCH -c 100
#SBATCH -t 24:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg 

bwa-mem2 index -p tonsa_genome/a_ton_index tonsa_genome/hic_output.fasta
