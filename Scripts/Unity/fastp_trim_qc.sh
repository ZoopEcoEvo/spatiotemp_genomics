#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

#for loop gets unique sample names from demultiplexed files
for file in demux_fastq/*R1.fq.gz; do

base=$(basename $file .R1.fq.gz)

fastp -i demux_fastq/$base.R1.fq.gz -I demux_fastq/$base.R2.fq.gz\
	-o cleaned_fastq/$base.R1.fq.gz -O cleaned_fastq/$base.R2.fq.gz\
	-h results/fastp_results/$base.html -j results/fastp_results/$base.json

#mv *.html results/fastp_results
#mv *.json results/fastp_results

done
