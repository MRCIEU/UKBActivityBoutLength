
# Generating activity bouts


## Installing package used to process CWA files

I use the software package created by Doherty (here)[https://github.com/activityMonitoring/biobankAccelerometerAnalysis].

Following Doherty's documentation, and adding modules on Blue Crystal, I installed this software using:

```bash
bash utilities/downloadDataModels.sh
module add languages/python-anaconda3-5.2.0
pip3 install --user .
module add languages/java-jdk-1.8.1-44
javac -cp java/JTransforms-3.1-with-dependencies.jar java/*.java
```

## Generating samples to process in each job part

```bash
matlab -r idSublists
```


## Generate activity bouts by processing the activity data


In the `job-bouts` directory:

```bash
qsub job-part1.sh
qsub job-part2.sh
```


Files generated:

activity-summary.txt
- the number of valid days in the complete days and imputed versions


For each participant:

bouts100-CD-[id].csv
bouts100-imp-[id].csv
boutsClass-CD-[id].csv
boutsClass-imp-[id].csv

Each contains the columns:
1. bout index
2. bout duration (seconds)
3. average area under the curve per minute (mg/minute)
4. complexity
5. check for valid bout
6. indicator for partial bout (start or end might not be the true start/end)
7. start time
8. end time

Note that the complete days bouts might not be an exact subset of the imputed bouts because an imputed bout might include non-imputed data, 
such that their is a shorter version of this bout (with the non-imputed data only) in the complete days bouts data (e.g. across two days, one
which is imputed and the other that isn't).


ID lists showing which participants are included in each sample:
cd-ids-inc.txt
cd-ids-exc.txt
imp-ids-inc.txt
imp-ids-exc.txt




## Checking

Check there are 93010 participants with bout data:

```bash
grep -v id ${PROJECT_DATA}/accel/derived/activityBoutsMAIN/boutssample*/activity-summary.txt  | wc -l
```


