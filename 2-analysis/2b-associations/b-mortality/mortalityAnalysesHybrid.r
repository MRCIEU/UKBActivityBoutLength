source('../../loadData.r')
source('../generic-functions/getConfounders.r')
source('../generic-functions/getConfounderVariables.r')
source('../generic-functions/getConfounderVariablesAll.r')

options(warn=1)

source('coxAssoc.r')
source('overallAssocs.r')
source('sedentaryBoutLengthAnalysisHybrid.r')
source('mvpaBoutLengthAnalysisHybrid.r')


mortalityAnalysesHybrid <- function(version) {

sink(paste0(Sys.getenv('RES_DIR'), '/mortality-assoc-log-hybrid-',version,'.txt'))

library("survival")
#library("survminer")

dataCD = loadData(version, TRUE)
confs = getConfounderVariables(dataCD)
confsAll = getConfounderVariablesAll(dataCD)

# convert to average per day
dataCD$dur1sed = dataCD$dur1sed/dataCD$num_days
dataCD$dur2sed = dataCD$dur2sed/dataCD$num_days
dataCD$dur3sed = dataCD$dur3sed/dataCD$num_days
dataCD$overall_classSleep = dataCD$overall_classSleep/dataCD$num_days
dataCD$overall_classLight = dataCD$overall_classLight / dataCD$num_days
dataCD$overall_classSed = dataCD$overall_classSed / dataCD$num_days
dataCD$overall_100mg = dataCD$overall_100mg / dataCD$num_days

dataCD$dur1mod100 = dataCD$dur1mod100/dataCD$num_days
dataCD$dur2mod100 = dataCD$dur2mod100/dataCD$num_days
dataCD$dur3mod100 = dataCD$dur3mod100/dataCD$num_days
dataCD$dur4mod100 = dataCD$dur4mod100/dataCD$num_days


#datadir=Sys.getenv('PROJECT_DATA');
#write.table(cbind(dataCD, confs), paste0(datadir, '/data-for-stata-het-test.csv'), row.names=FALSE, sep=',', quote=FALSE)
#stop()




#####
##### Overall associations - swapping between activity categories

print('###############################################')
print('###########     OVERALL ASSOCS     ############')
print('###############################################')





overallAssocs(dataCD, confs, confsAll, version, TRUE)




######
###### Sedentary bout strata analysis


print('###############################################')
print('#########   SED BOUT LENGTH STRATA   ##########')
print('###############################################')


sedentaryBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version)


######
###### Moderate-vigorous bout strata analysis


print('###############################################')
print('#########   MOD BOUT LENGTH STRATA   ##########')
print('###############################################')


mvpaBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version)





print('###############################################')
print('#########   SENSTIVITY - 1 YEAR      ##########')
print('###############################################')


## sensitivity excluding those who died before 1 year after accel wear

# shift survival start 1 year
dataCD$survivalStartAge = dataCD$survivalStartAge + 1

# remove those already died
ix = which(dataCD$survivalStartAge >= dataCD$survivalEndAge)
dataCD = dataCD[-ix,]
confs = confs[-ix,]
confsAll = confsAll[-ix,]

version1 = paste0(version, 'sensitivity1')

overallAssocs(dataCD, confs, confsAll, version1, TRUE)
sedentaryBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version1)
mvpaBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version1)




print('###############################################')
print('#########   SENSTIVITY - 2 YEAR      ##########')
print('###############################################')


## sensitivity excluding those who died before 2 years after accel wear

# shift survival start 1 year more
dataCD$survivalStartAge = dataCD$survivalStartAge + 1

# remove those already died
ix = which(dataCD$survivalStartAge >= dataCD$survivalEndAge)
dataCD = dataCD[-ix,]
confs = confs[-ix,]
confsAll = confsAll[-ix,]

version2 = paste0(version, 'sensitivity2')

overallAssocs(dataCD, confs, confsAll, version2, TRUE)
sedentaryBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version2)
mvpaBoutLengthAnalysisHybrid(dataCD, confs, confsAll, version2)














sink()

}



mortalityAnalysesHybrid('CD')

mortalityAnalysesHybrid('imp')
