dataDir = Sys.getenv('PROJECT_DATA') 
source('loadData.r')
library(data.table)


##
## complete data, predicted

dataCDpred = loadData('CD', FALSE)
colsx = colnames(dataCDpred)
ix = which(colsx == "eid" | colsx %like% "dur" | colsx %like% "overall_" | colsx %like% "n_" | colsx == "num_complete_days" | colsx == "completeDaysAVM")
dataCDpred = dataCDpred[,ix]
colnames(dataCDpred) = gsub("__", "_", colnames(dataCDpred))
dataCDpred$dur4sed = NULL
write.table(dataCDpred, paste0(dataDir, '/derived-for-ukb/bouts-cd-not-hybrid.csv'), sep=',', col.names=TRUE, row.names=FALSE)

##
## complete data, hybrid

dataCDhybrid = loadData('CD', TRUE)
colsx = colnames(dataCDhybrid)
ix = which(colsx == "eid" | colsx %like% "dur" | colsx %like% "overall_" | colsx %like% "n_" | colsx == "num_complete_days" | colsx == "completeDaysAVM")
dataCDhybrid = dataCDhybrid[,ix]
colnames(dataCDhybrid) = gsub("__", "_", colnames(dataCDhybrid))
dataCDhybrid$dur4sed = NULL
write.table(dataCDhybrid, paste0(dataDir, '/derived-for-ukb/bouts-cd-hybrid.csv'), sep=',', col.names=TRUE, row.names=FALSE)


##
## imputed data, predicted

dataImppred = loadData('imp', FALSE)
colsx = colnames(dataImppred)
ix = which(colsx == "eid" | colsx %like% "dur" | colsx %like% "overall_" | colsx %like% "n_" | colsx == "num_imputed_days" | colsx == "imputedAVM")
dataImppred = dataImppred[,ix]
colnames(dataImppred) = gsub("__", "_", colnames(dataImppred))
dataImppred$dur4sed = NULL
write.table(dataImppred, paste0(dataDir, '/derived-for-ukb/bouts-imp-not-hybrid.csv'), sep=',', col.names=TRUE, row.names=FALSE)



##
## imputed data, hybrid

dataImphybrid = loadData('imp', TRUE)
colsx = colnames(dataImphybrid)
ix = which(colsx == "eid" | colsx %like% "dur" | colsx %like% "overall_" | colsx %like% "n_" | colsx == "num_imputed_days" | colsx == "imputedAVM")
dataImphybrid = dataImphybrid[,ix]
colnames(dataImphybrid) = gsub("__", "_", colnames(dataImphybrid))
dataImphybrid$dur4sed = NULL
write.table(dataImphybrid, paste0(dataDir, '/derived-for-ukb/bouts-imp-hybrid.csv'), sep=',', col.names=TRUE, row.names=FALSE)



##
## FURTHER CHECKING OF DERIVED DATA
##


checkData <- function(x) {

library(data.table)

# check bout variables equal total number of minutes in valid days
colsx = colnames(x)
ix = which(colsx %like% "dur")
sumdur = rowSums(x[,ix])
summins = x$num_complete_days*1440
ix = which(sumdur!=summins & sumdur!=(summins-1))
print(length(ix))

# check same but for each activity category

ix = which(colsx %like% "dur.*classSleep")
sumx = rowSums(x[,ix])
ix = which(sumx!=x$overall_classSleep & sumx!=(x$overall_classSleep-1))
print(length(ix))

ix = which(colsx %like% "dur.*sed")
sumx = rowSums(x[,ix])
ix = which(sumx!=x$overall_classSed & sumx!=(x$overall_classSed-1))
print(length(ix))

ix = which(colsx %like% "dur.*classLight")
sumx = rowSums(x[,ix])
ix = which(sumx!=x$overall_classLight & sumx!=(x$overall_classLight-1))
print(length(ix))

ix = which(colsx %like% "dur.*mod100")
sumx = rowSums(x[,ix])
ix = which(sumx!=x$overall_100mg & sumx!=(x$overall_100mg-1))
print(length(ix))

}




x = read.table(paste0(dataDir, '/derived-for-ukb/bouts-cd-not-hybrid.csv'), header=1, sep=',')
checkData(x)

x = read.table(paste0(dataDir, '/derived-for-ukb/bouts-cd-hybrid.csv'), header=1, sep=',')
checkData(x)

x = read.table(paste0(dataDir, '/derived-for-ukb/bouts-imp-not-hybrid.csv'), header=1, sep=',')
checkData(x)

x = read.table(paste0(dataDir, '/derived-for-ukb/bouts-imp-hybrid.csv'), header=1, sep=',')
checkData(x)

