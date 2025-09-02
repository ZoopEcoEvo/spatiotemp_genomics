#!/bin/bash
#SBATCH -c 50
#SBATCH -t 6:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

for file in bam_files/*_sorted.bam; do

base=$(basename $file _sorted.bam) #using this file to ensure just one file per sample selected

echo "Calculating alignment metrics for $base"

#java -jar /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg/share/picard-3.4.0-0/picard.jar CollectWgsMetrics -I bam_files/$base'_sorted.bam' -O results/align_metrics/$base'_wgs_metrics.txt' -R tonsa_genome/hic_output.fasta
java -jar /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg/share/picard-3.4.0-0/picard.jar CollectAlignmentSummaryMetrics -I bam_files/$base'_sorted.bam' -O results/align_metrics/$base'_map_metrics.txt' -R tonsa_genome/hic_output.fasta

done
