#!/bin/bash
#SBATCH -c 100
#SBATCH -t 24:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg
module load conda/latest
conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

for i in {1..5}

do

fqtk demux --inputs combined_plate${i}/*.fq.gz --read-structures 8B12S+T 8S+T --sample-metadata combined_plate${i}/twist_map.tsv --output demux_fastq/

#IMPORTANT - rename the output text file summary for each plate BEFORE running fqtk for the next plate
mv demux_fastq/demux-metrics.txt demux_fastq/plate${i}_metrics.txt

echo plate${i} reads separated out into individual samples

done
