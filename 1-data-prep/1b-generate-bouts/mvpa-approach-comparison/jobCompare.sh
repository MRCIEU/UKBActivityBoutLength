#!/bin/bash
#PBS -l walltime=60:00:00,nodes=1:ppn=4
#PBS -o outputComp
#PBS -e errorsComp
#---------------------------------------------

cd $PBS_O_WORKDIR

sh processAccelMVPACompare.sh
