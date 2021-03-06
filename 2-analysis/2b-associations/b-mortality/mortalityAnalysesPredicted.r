source('../../loadData.r')
source('../generic-functions/getConfounders.r')
source('../generic-functions/getConfounderVariables.r')
source('../generic-functions/getConfounderVariablesAll.r')

source('coxAssoc.r')
source('overallAssocs.r')
source('sedentaryBoutLengthAnalysisPredicted.r')
source('mvpaBoutLengthAnalysisPredicted.r')

mortalityAnalysesPredicted <- function(version) {

sink(paste0(Sys.getenv('RES_DIR'), '/mortality-assoc-log-predicted-',version,'.txt'))


library("survival")
library("survminer")
 
dataCD = loadData(version)
confs = getConfounderVariables(dataCD)
confsAll = getConfounderVariablesAll(dataCD)


# convert to average per day
dataCD$dur1sed = dataCD$dur1sed/dataCD$num_days
dataCD$dur2sed = dataCD$dur2sed/dataCD$num_days
dataCD$dur3sed = dataCD$dur3sed/dataCD$num_days
dataCD$overall_classSleep = dataCD$overall_classSleep/dataCD$num_days
dataCD$overall_classLight = dataCD$overall_classLight / dataCD$num_days
dataCD$overall_classMod = dataCD$overall_classMod / dataCD$num_days
dataCD$overall_classWalk = dataCD$overall_classWalk / dataCD$num_days
dataCD$overall_classSed = dataCD$overall_classSed / dataCD$num_days


dataCD$dur1mod = dataCD$dur1mod/dataCD$num_days
dataCD$dur2mod = dataCD$dur2mod/dataCD$num_days
dataCD$dur3mod = dataCD$dur3mod/dataCD$num_days
dataCD$dur4mod = dataCD$dur4mod/dataCD$num_days




#####
##### Overall associations - swapping between activity categories

print('###############################################')
print('###########     OVERALL ASSOCS     ############')
print('###############################################')


overallAssocs(dataCD, confs, confsAll, version)




######
###### Sedentary bout strata analysis


print('###############################################')
print('#########   SED BOUT LENGTH STRATA   ##########')
print('###############################################')


sedentaryBoutLengthAnalysisPredicted(dataCD, confs, confsAll, version)



######
###### Moderate-vigorous bout strata analysis


print('###############################################')
print('#########   MOD BOUT LENGTH STRATA   ##########')
print('###############################################')


mvpaBoutLengthAnalysisPredicted(dataCD, confs, confsAll, version)


sink()

}



mortalityAnalysesPredicted('CD')
mortalityAnalysesPredicted('imp')


