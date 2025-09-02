#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

#for loop gets unique sample names from cleaned and trimmed files
for file in cleaned_fastq/*R1.fq.gz; do

base=$(basename $file .R1.fq.gz)

echo aligning $base to reference

header=$(zcat $file | head -n 1)
id=$(echo $header | head -n 1 | cut -f 1-4 -d":" | sed 's/@//' | sed 's/:/_/g')

#echo "Read Group @RG\tID:$id\tSM:$id"_"$base\tLB:twist_1\tPL:ILLUMINA"

bwa-mem2 mem -t 100 -R $(echo "@RG\tID:$id\tSM:$id"_"$base\tLB:twist_1\tPL:ILLUMINA") tonsa_genome/a_ton_index cleaned_fastq/$base.R1.fq.gz cleaned_fastq/$base.R2.fq.gz > sam_files/$base.sam

done
