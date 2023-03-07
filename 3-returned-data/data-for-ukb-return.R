
## CD hybrid

cleanData <- function(x, cd, hybrid) {

  print(dim(x))
  
  ##
  ## keep only fields for - eid, avm, num days, bout durations, overall durations
  
  # find col indexes of these fields
  if (cd == TRUE) {
  ix = which(colnames(x) %in% c('eid', 'completeDaysAVM', 'num_complete_days'))
  }
  else {
    ix = which(colnames(x) %in% c('eid', 'imputedAVM', 'num_imputed_days'))
  }
  
  ixOverall <- which(startsWith(colnames(x), 'overall'))
  ixDur <- which(startsWith(colnames(x), 'dur'))
  
  # reduce data frame to just these fields
  allcolumnIdx = c(ix, ixOverall, ixDur)
  x <- x[,allcolumnIdx]
  
  ## checking
  assertthat::are_equal(x$overall_classSed, rowSums(x[,c('dur1sed', 'dur2sed', 'dur3sed')]))
  assertthat::are_equal(x$overall_classSleep, rowSums(x[,c('dur1_classSleep', 'dur2_classSleep', 'dur3_classSleep', 'dur4_classSleep')]))
  assertthat::are_equal(x$overall_classLight, rowSums(x[,c('dur1_classLight', 'dur2_classLight', 'dur3_classLight', 'dur4_classLight')]))
  
  if (hybrid == FALSE) {
    assertthat::are_equal(x$overall_classWalk, rowSums(x[,c('dur1_classWalk', 'dur2_classWalk', 'dur3_classWalk', 'dur4_classWalk')]))
    assertthat::are_equal(x$overall_classMod, rowSums(x[,c('dur1mod', 'dur2mod', 'dur3mod', 'dur4mod')]))
  }
  else {
    assertthat::are_equal(x$overall_100mg, rowSums(x[,c('dur1mod100', 'dur2mod100', 'dur3mod100', 'dur4mod100')]))
  }
  
  # make column names for specific data version cd/imp and hybrid/nothybrid
  if (hybrid == TRUE) {
    colnames(x) = paste0("hybrid_", colnames(x))
  }
  else {
    colnames(x) = paste0("nothybrid_", colnames(x))
  }
  
  if (cd == TRUE) {
    colnames(x) = paste0("CD_", colnames(x))
  }
  else {
    colnames(x) = paste0("imp_", colnames(x))
  }
  
  return(x)
}

dataCDHybrid = read.table('bouts-cd-hybrid.csv', sep=',', header=1)
dataCDHybrid = cleanData(dataCDHybrid, 1, 1)

dataImpHybrid = read.table('bouts-imp-hybrid.csv', sep=',', header=1)
dataImpHybrid = cleanData(dataImpHybrid, 0, 1)

dataCDNotHybrid = read.table('bouts-cd-not-hybrid.csv', sep=',', header=1)
dataCDNotHybrid = cleanData(dataCDNotHybrid, 1, 0)

dataImpNotHybrid = read.table('bouts-imp-not-hybrid.csv', sep=',', header=1)
dataImpNotHybrid = cleanData(dataImpNotHybrid, 0, 0)

colnames(dataCDHybrid)
colnames(dataImpHybrid)
colnames(dataCDNotHybrid)
colnames(dataImpNotHybrid)

combinedData = merge(dataCDHybrid, dataCDNotHybrid, by.x="CD_hybrid_eid", by.y="CD_nothybrid_eid", all=TRUE, sort=FALSE)
dim(combinedData)
combinedData = merge(combinedData, dataImpHybrid, by.x="CD_hybrid_eid", by.y="imp_hybrid_eid", all=TRUE, sort=FALSE)
dim(combinedData)
combinedData = merge(combinedData, dataImpNotHybrid, by.x="CD_hybrid_eid", by.y="imp_nothybrid_eid", all=TRUE, sort=FALSE)
dim(combinedData)

####
#### clean up duplicated columns

## AVM - only one for CD and one for imp needed
# CD
assertthat::are_equal(combinedData$CD_nothybrid_completeDaysAVM, combinedData$CD_hybrid_completeDaysAVM)
combinedData$completeDaysAVM = combinedData$CD_nothybrid_completeDaysAVM
combinedData$CD_hybrid_completeDaysAVM = NULL
combinedData$CD_nothybrid_completeDaysAVM = NULL
# imp
assertthat::are_equal(combinedData$imp_nothybrid_imputedAVM, combinedData$imp_hybrid_completeDaysAVM)
combinedData$imputedAVM = combinedData$imp_nothybrid_imputedAVM
combinedData$imp_nothybrid_imputedAVM = NULL
combinedData$imp_hybrid_imputedAVM = NULL

## num days
assertthat::are_equal(combinedData$CD_hybrid_num_complete_days, combinedData$CD_nothybrid_num_complete_days)
assertthat::are_equal(combinedData$imp_hybrid_num_imputed_days, combinedData$imp_nothybrid_num_imputed_days)

combinedData$num_complete_days = combinedData$CD_hybrid_num_complete_days
combinedData$CD_hybrid_num_complete_days = NULL
combinedData$CD_nothybrid_num_complete_days = NULL

combinedData$num_imputed_days = combinedData$imp_hybrid_num_imputed_days
combinedData$imp_hybrid_num_imputed_days = NULL
combinedData$imp_nothybrid_num_imputed_days = NULL

colnames(combinedData)

colnames(combinedData)[which(colnames(combinedData) == "CD_hybrid_eid")] = "eid"


write.csv(combinedData, 'ukb-17810-returned.csv', row.names=FALSE)


