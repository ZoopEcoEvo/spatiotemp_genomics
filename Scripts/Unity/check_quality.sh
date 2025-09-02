#!/bin/bash
#SBATCH -c 100
#SBATCH -t 24:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg


for file in demux_fastq/*.fq.gz; do
    if [ -f "$file" ]; then
        fastqc -o results/fastqc_seq_quality/ $file
    fi
done


cp results/fastqc_seq_quality/*.html /work/pi_matthew_sasaki_uml_edu/matt/Aton_LCWG/02.Reports/Fastqc/
