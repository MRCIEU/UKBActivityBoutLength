
# for epoch generation
#module add languages/python-3.3.2
#module add languages/java-jdk-1.8.1-44

codeDir="${HOME}/2016-biobank-accelerometer/code/ActivityBouts/1-data-prep/1b-generate-bouts/"

cd $codeDir

date

# arg1: listing of user ids
jidx="${1}"
fname="sample${jidx}"



tmpdir="${HOME}/tmp/${fname}/"
#rm -f -r $tmpdir
mkdir $tmpdir
chmod 700 $tmpdir

idFile="${HOME}/2016-biobank-accelerometer/data/accel/samples/${fname}.csv"


## make the subdirectory for the bout listings

boutsdir="${HOME}/2016-biobank-accelerometer/data/accel/derived/activityBouts/bouts${fname}/"
mkdir $boutsdir
mkdir "${boutsdir}errors/"

## Aiden Doherty's code (https://github.com/activityMonitoring/biobankAccelerometerAnalysis)
epochCodeDir="${HOME}/2016-biobank-accelerometer/code/biobankAccelerometerAnalysis-master/"


Rscript runBoutGeneration.r "${idFile}" "${epochCodeDir}" "${tmpdir}" "${UKB_ACCEL}" "$boutsdir" 60 "${2}"

