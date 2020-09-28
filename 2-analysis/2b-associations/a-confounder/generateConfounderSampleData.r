
dataDir = Sys.getenv('PROJECT_DATA')

source('../../loadData.r')

# load sample
insampledata = loadData('CD')

# set flag for those in sample
insampledata$insample = 1
print(dim(insampledata))
insampledata = insampledata[,c('eid', 'insample'), drop=FALSE]


# load original data
origdata = read.table(paste(dataDir, '/phenotypes/derived/analysis-dataset-39441-39542-newdeath.csv', sep=''), header=1, sep=',')
print(dim(origdata))
origdata = origdata[which(!is.na(origdata$accelaccept)),]
print(dim(origdata))




# merge original and sample data
alldata = merge(insampledata, origdata, sort=FALSE, by='eid', all=TRUE)
alldata = alldata[, c('eid', 'age', 'sex', 'education0', 'education1', 'education2', 'education3', 'education4', 'education5', 'ethnicity', 'bmi', 'datedeath0', 'insample', 'accelaccept', 'townsend', 'accelwearstartdate', 'smokestatus', 'income', 'numillness', 'numillnesscancer')]
print(dim(alldata))


# set flag for those not in sample
ix = which(is.na(alldata$insample))
alldata$insample[ix] = 0

# set flag for those that were invited but did not accept accelerometer wear
alldata$insample[which(alldata$accelaccept == 0)] = 0

print(table(alldata$insample))


source('../generic-functions/getConfounders.r')
alldata$accelwearstartdate = strptime(alldata$accelwearstartdate, format='%Y-%m-%dT%H:%M:%S')
confs = getConfounders(alldata)

alldata = cbind(alldata, confs)

# save data with insample flag
outfile = paste0(dataDir, '/data-for-confounder-table-39441-39542-newdeath.csv')
write.table(alldata, outfile, row.names=FALSE, sep=',')




