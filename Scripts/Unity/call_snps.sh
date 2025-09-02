#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-Aton_lcwg/

module load conda/latest

conda activate /work/pi_matthew_sasaki_uml_edu/matt-conda/envs/lcwg

angsd -b lists/bam_list.txt -ref tonsa_genome/hic_output.fasta -out results/angsd_tonsa_exclusions/snp_call -GL 1 -doGlf 2 -doMaf 1 -doMajorMinor 1 -doCounts 1 -doDepth 1 -maxDepth 10000 -dumpCounts 1 -doIBS 1 -makematrix 1 -doCov 1 -setMinDepth 144 -setMaxDepth 10000 -minInd 218 -minQ 20 -minMapQ 20 -SNP_pval 1e-6 -minMaf 0.05 -P 100 >& results/angsd_tonsa_exclusions/angsd_snp_call.log

echo "Done with ANGSD part 1"

## Create a SNP list to use in downstream analyses
gunzip -c results/angsd_tonsa_exclusions/snp_call.mafs.gz | cut -f 1,2,3,4 | tail -n +2 > results/angsd_tonsa_exclusions/global_snp_list.txt

angsd sites index results/angsd_tonsa_exclusions/global_snp_list.txt

echo "Done with ANGSD part 2"

## Also make it in regions format for downstream analyses
cut -f 1,2 results/angsd_tonsa_exclusions/global_snp_list.txt | sed 's/\t/:/g' > results/angsd_tonsa_exclusions/global_snp_list.regions

## Lastly, extract a list of chromosomes/LGs/scaffolds for downstream analysis
cut -f1 results/angsd_tonsa_exclusions/global_snp_list.txt | sort | uniq > results/angsd_tonsa_exclusions/global_snp_list.chrs

echo "Done with extra formatting"

