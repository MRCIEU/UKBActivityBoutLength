#!/bin/bash
#PBS -l walltime=5:00:00,nodes=1:ppn=1
#PBS -o outputAgg
#PBS -e errorsAgg
#PBS -t 1-94
#---------------------------------------------

cd $PBS_O_WORKDIR

date


pIdx=${PBS_ARRAYID}
numx=$((((pIdx-1)*1000)+1))


echo $numx


Rscript aggregateBoutsMain.r CD predicted $numx

date
