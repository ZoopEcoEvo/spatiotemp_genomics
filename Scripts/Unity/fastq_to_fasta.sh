#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

#for loop gets unique sample names from demultiplexed files
for file in cleaned_fastq/*.fq.gz; do

base=$(basename $file .fq.gz)

gunzip -c $file | sed -n '1~4s/^@/>/p;2~4p' > clade_ids/$base.fasta

echo "Done with $base"

done
