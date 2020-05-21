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
library("survminer")
 
dataCD = loadData(version, TRUE)
confs = getConfounderVariables(dataCD)
confsAll = getConfounderVariablesAll(dataCD)

dataCD$dur1sed = dataCD$dur1_auc1__classSed + dataCD$dur1_auc2__classSed + dataCD$dur1_auc3__classSed
dataCD$dur2sed = dataCD$dur2_auc1__classSed + dataCD$dur2_auc2__classSed + dataCD$dur2_auc3__classSed
dataCD$dur3sed = dataCD$dur3_auc1__classSed + dataCD$dur3_auc2__classSed + dataCD$dur3_auc3__classSed

# convert to average per day
dataCD$dur1sed = dataCD$dur1sed/dataCD$num_days
dataCD$dur2sed = dataCD$dur2sed/dataCD$num_days
dataCD$dur3sed = dataCD$dur3sed/dataCD$num_days
dataCD$overall_classSleep = dataCD$overall_classSleep/dataCD$num_days
dataCD$overall_classLight = dataCD$overall_classLight / dataCD$num_days
dataCD$overall_classSed = dataCD$overall_classSed / dataCD$num_days
dataCD$overall_100mg = dataCD$overall_100mg / dataCD$num_days

dataCD$dur1mod100 = dataCD$dur1_auc1__100mg + dataCD$dur1_auc2__100mg + dataCD$dur1_auc3__100mg
dataCD$dur2mod100 = dataCD$dur2_auc1__100mg + dataCD$dur2_auc2__100mg + dataCD$dur2_auc3__100mg
dataCD$dur3mod100 = dataCD$dur3_auc1__100mg + dataCD$dur3_auc2__100mg + dataCD$dur3_auc3__100mg
dataCD$dur1mod100 = dataCD$dur1mod100/dataCD$num_days
dataCD$dur2mod100 = dataCD$dur2mod100/dataCD$num_days
dataCD$dur3mod100 = dataCD$dur3mod100/dataCD$num_days


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


sink()

}



mortalityAnalysesHybrid('CD')

mortalityAnalysesHybrid('imp')
