#!/bin/bash
#SBATCH -c 100
#SBATCH -t 48:00:00
#SBATCH --mail-type=ALL

cd /scratch/workspace/matthew_sasaki_uml_edu-aton_lcwg/

FST_ROOT="results/angsd/fst"
OUTFILE="results/angsd/fst/fst_summary.tsv"

echo -e "FILE\tCLADE\tSITE1\tSEASON1\tSITE2\tSEASON2\tCOMPARISON\tFST_weight\tFST_unweight" > $OUTFILE

for CLADE_DIR in ${FST_ROOT}/clade_*
do

    for FILE in ${CLADE_DIR}/*.global.fst
    do

        BASENAME=$(basename $FILE .global.fst)
        # Ex base name - F_MR_late_vs_F_MR_peak

        # extract fst value (usually first number in file)
        FST1=$(cut -f 1 $FILE)
        FST2=$(cut -f 2 $FILE)

        ####################################
        # TEMPORAL COMPARISON
        ####################################

            CLADE1=$(echo $BASENAME | cut -d'_' -f1)
            SITE1=$(echo $BASENAME | cut -d'_' -f2)
            SEASON1=$(echo $BASENAME | cut -d'_' -f3)
            CLADE2=$(echo $BASENAME | cut -d'_' -f5)
            SITE2=$(echo $BASENAME | cut -d'_' -f6)
            SEASON2=$(echo $BASENAME | cut -d'_' -f7)

			if [ "$SITE1" == "$SITE2" ]; then

				COMP="seasonal"
			
				else
			
				COMP="spatial"

			fi

            echo -e "${BASENAME}\t${CLADE1}\t${SITE1}\t${SEASON1}\t${SITE2}\t${SEASON2}\t${COMP}\t${FST1}\t${FST2}" >> $OUTFILE

    done

done

echo "FST summary written to $OUTFILE"
