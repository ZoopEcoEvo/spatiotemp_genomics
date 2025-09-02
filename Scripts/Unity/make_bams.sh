#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL


cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

#for loop gets unique sample names from demultiplexed files
for file in sam_files/*.sam; do

base=$(basename $file .sam)

echo "Working with $base"

samtools view -b -F 4 -@90 -o bam_files/$base.bam sam_files/$base.sam

# Filter bam files to remove poorly mapped reads (non-unique mappings and mappings with a quality score < 20)
samtools view -h -q 20 -@90 bam_files/$base.bam | samtools view -@90 -buS - | samtools sort -@90 -o bam_files/$base'_sorted.bam'


done
