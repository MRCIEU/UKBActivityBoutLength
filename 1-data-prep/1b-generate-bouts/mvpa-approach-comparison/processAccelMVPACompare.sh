
# for epoch generation
#module add languages/python-3.3.2
#module add languages/java-jdk-1.8.1-44

date


idFile="${HOME}/2016-biobank-accelerometer/data/accel/derived/mvpaCompare/ids.csv"


## make the subdirectory for the bout listings

boutsdir="${HOME}/2016-biobank-accelerometer/data/accel/derived/mvpaCompare/"
tmpdir="${boutsdir}tmp/"

mkdir "${boutsdir}errors/"

## Aiden Doherty's code (https://github.com/activityMonitoring/biobankAccelerometerAnalysis)
epochCodeDir="${HOME}/2016-biobank-accelerometer/code/biobankAccelerometerAnalysis-master/"


Rscript runMVPACompare.r "${idFile}" "${epochCodeDir}" "${tmpdir}" "${UKB_ACCEL}" "$boutsdir" 60


date

