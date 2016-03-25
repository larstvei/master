#!/bin/bash

# Job name:
#SBATCH --job-name=larstvei
#
# Project:
#SBATCH --account=uio
#
# Wall clock limit:
#SBATCH --time=168:00:00
#
# Max memory usage:
#SBATCH --mem-per-cpu=4G
#
# Number of cores:
#SBATCH --cpus-per-task=1

## Set up job environment:
source /cluster/bin/jobsetup
module purge   # clear any inherited modules
set -o errexit # exit on errors

MAUDE_PROGRAM=$1
MODE_EXE=$SCRATCH/maude/maude.linux64
LOG_FILE=$SCRATCH/sim_results/$(date +"%Y-%m-%d-%H:%M")

cp -r nobackup/maude $SCRATCH
cp $MAUDE_PROGRAM $SCRATCH

cd $SCRATCH

mkdir -p sim_results

cleanup "cp $LOG_FILE /usit/abel/u1/larstvei/sim_results/"

echo "Running!"
(
time -p $MODE_EXE -no-banner -no-advise -no-wrap -batch <<EOF
load $MAUDE_PROGRAM
red modelCheck(init (g h) 3, []legal(g h)) .
quit
EOF
) > $LOG_FILE

echo "Done!"

scancel $SLURM_JOB_ID
