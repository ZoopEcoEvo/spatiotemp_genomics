#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

for file in bam_files/*_sorted.bam; do

base=$(basename $file _sorted.bam)

echo "Calculating depths for $base"
samtools depth -aa bam_files/$base'_sorted.bam' -q 20 -Q 20 | cut -f 3 | gzip > bam_files/$base.depth.gz

done
