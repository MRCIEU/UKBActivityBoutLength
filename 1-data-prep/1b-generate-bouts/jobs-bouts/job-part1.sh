#!/bin/bash
#PBS -l walltime=200:00:00,nodes=1:ppn=1
#PBS -o output
#PBS -e errors
#PBS -t 1-50%20
#---------------------------------------------

cd $PBS_O_WORKDIR
cd ..

jidx=$(((1000*${PBS_ARRAYID})-999))

echo $jidx

fname="sample${jidx}"
sh processAccel.sh "$jidx" "FALSE"
