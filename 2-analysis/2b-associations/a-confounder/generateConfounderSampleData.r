
dataDir = Sys.getenv('PROJECT_DATA')

source('../../loadData.r')

# load sample
insampledata = loadData('CD')

# set flag for those in sample
insampledata$insample = 1
print(dim(insampledata))
insampledata = insampledata[,c('eid', 'insample'), drop=FALSE]


# load original data
origdata = read.table(paste(dataDir, '/phenotypes/derived/analysis-dataset-43777-all.csv', sep=''), header=1, sep=',')
print(dim(origdata))
origdata = origdata[which(!is.na(origdata$accelaccept)),]
print(dim(origdata))




# merge original and sample data
alldata = merge(insampledata, origdata, sort=FALSE, by='eid', all=TRUE)


####
####

# make new disease incidence variables occuring before first date of accelerometer wear across all participants

firstAccelDate = min(as.POSIXct(strptime(alldata$accelwearstartdate, "%Y-%m-%d %H:%M:%S")), na.rm=TRUE)

print(paste0('First date of accelerometer wear: ', firstAccelDate))

print('make circulatory disease var')

phenos = alldata
phenos$prev_firstocc_circ = 0
circPhenos = c("X131270.0.0","X131272.0.0","X131274.0.0","X131276.0.0","X131278.0.0","X131280.0.0","X131282.0.0","X131284.0.0","X131286.0.0","X131288.0.0","X131290.0.0","X131292.0.0","X131294.0.0","X131296.0.0","X131298.0.0","X131300.0.0","X131302.0.0","X131304.0.0","X131306.0.0","X131308.0.0","X131310.0.0","X131312.0.0","X131314.0.0","X131316.0.0","X131318.0.0","X131320.0.0","X131322.0.0","X131324.0.0","X131326.0.0","X131328.0.0","X131330.0.0","X131332.0.0","X131334.0.0","X131336.0.0","X131338.0.0","X131340.0.0","X131342.0.0","X131344.0.0","X131346.0.0","X131348.0.0","X131350.0.0","X131352.0.0","X131354.0.0","X131356.0.0","X131358.0.0","X131360.0.0","X131362.0.0","X131364.0.0","X131366.0.0","X131368.0.0","X131370.0.0","X131372.0.0","X131374.0.0","X131376.0.0","X131378.0.0","X131380.0.0","X131382.0.0","X131384.0.0","X131386.0.0","X131388.0.0","X131390.0.0","X131392.0.0","X131394.0.0","X131396.0.0","X131398.0.0","X131400.0.0","X131402.0.0","X131404.0.0","X131406.0.0","X131408.0.0","X131410.0.0","X131412.0.0","X131414.0.0","X131416.0.0","X131418.0.0","X131420.0.0","X131422.0.0")
for (i in 1: length(circPhenos)) { 
	nx = circPhenos[i]
	print(nx)
	x = as.POSIXct(strptime(phenos[,nx], format='%Y-%m-%d')) 
	ixCirc = which(difftime(x,firstAccelDate)<0)
	phenos$prev_firstocc_circ[ixCirc] = 1
}



##
## respiratory: identify those alread with resp system disease before accel wear

print('make respiratory disease var')

respPhenos = c("X131424.0.0","X131426.0.0","X131428.0.0","X131430.0.0","X131432.0.0","X131434.0.0","X131436.0.0","X131438.0.0","X131440.0.0","X131442.0.0","X131444.0.0","X131446.0.0","X131448.0.0","X131450.0.0","X131452.0.0","X131454.0.0","X131456.0.0","X131458.0.0","X131460.0.0","X131462.0.0","X131464.0.0","X131466.0.0","X131468.0.0","X131470.0.0","X131472.0.0","X131474.0.0","X131476.0.0","X131478.0.0","X131480.0.0","X131482.0.0","X131484.0.0","X131486.0.0","X131488.0.0","X131490.0.0","X131492.0.0","X131494.0.0","X131496.0.0","X131498.0.0","X131500.0.0","X131502.0.0","X131504.0.0","X131506.0.0","X131508.0.0","X131512.0.0","X131514.0.0","X131516.0.0","X131518.0.0","X131520.0.0","X131522.0.0","X131524.0.0","X131526.0.0","X131528.0.0","X131530.0.0","X131532.0.0","X131534.0.0","X131536.0.0","X131538.0.0","X131540.0.0","X131542.0.0","X131544.0.0","X131546.0.0","X131548.0.0","X131550.0.0")

phenos$prev_firstocc_resp = 0
for (i in 1: length(respPhenos)) {
        nx = respPhenos[i] 
        print(nx) 
        x = as.POSIXct(strptime(phenos[,nx], format='%Y-%m-%d'))  
	ixResp = which(difftime(x,firstAccelDate)<0)
        phenos$prev_firstocc_resp[ixResp] = 1
}

print(summary(phenos$prev_firstocc_resp))


##
## cancer: identify those alread with cancer before accel wear

# field 84 has ages and years
print('make cancer var')

cancerPhenos = c("X40005.0.0","X40005.1.0","X40005.2.0","X40005.3.0","X40005.4.0","X40005.5.0","X40005.6.0","X40005.7.0","X40005.8.0","X40005.9.0","X40005.10.0","X40005.11.0","X40005.12.0","X40005.13.0","X40005.14.0","X40005.15.0","X40005.16.0")

print(head(phenos[,cancerPhenos]))
phenos$prev_firstocc_canc = 0

for (i in 1: length(cancerPhenos)) {
	nx = cancerPhenos[i]
	x = as.POSIXct(strptime(phenos[,nx], format='%Y-%m-%d'))
	ixCanc = which(difftime(x,firstAccelDate)<0)
	phenos$prev_firstocc_canc[ixCanc] = 1
}

print(summary(phenos$prev_firstocc_canc))



alldata$prev_firstocc_resp = phenos$prev_firstocc_resp
alldata$prev_firstocc_circ = phenos$prev_firstocc_circ
alldata$prev_firstocc_canc = phenos$prev_firstocc_canc



#####
#####




alldata = alldata[, c('eid', 'age', 'sex', 'education0', 'education1', 'education2', 'education3', 'education4', 'education5', 'ethnicity', 'bmi', 'datedeath0', 'insample', 'accelaccept', 'townsend', 'accelwearstartdate', 'smokestatus', 'income', "prev_firstocc_circ","prev_firstocc_resp","prev_firstocc_canc", 'yearbirth')]
print(dim(alldata))


# set flag for those not in sample
ix = which(is.na(alldata$insample))
alldata$insample[ix] = 0

# set flag for those that were invited but did not accept accelerometer wear
#alldata$insample[which(alldata$accelaccept == 0)] = 0

print(table(alldata$insample))


source('../generic-functions/getConfounders.r')
#alldata$accelwearstartdate = strptime(alldata$accelwearstartdate, format='%Y-%m-%dT%H:%M:%S')

alldata$yearbirth = strptime(paste0(alldata$yearbirth,'-07-01'), format='%Y-%m-%d') 
alldata$accelwearstartdate = strptime(alldata$accelwearstartdate, format='%Y-%m-%d %H:%M:%S')
alldata$accelwearenddate = alldata$accelwearstartdate + 7*24*60*60
alldata$survivalStartDate = alldata$accelwearenddate
alldata$survivalStartAge = difftime(alldata$survivalStartDate, alldata$yearbirth, units="days")/365.25



# remove deaths after censor end data
censorDate = strptime('2019-12-31', format='%Y-%m-%d')
ixC = which(difftime(censorDate, alldata$datedeath0, units="days")<0)
print(paste0("number of participants who died after censor date: ", length(ixC)))
alldata$datedeath0[ixC] = NA

#alldata$deathoccur = 0
#alldata$deathoccur[which(!is.na(alldata$datedeath0))] = 1



## set up season indicators

alldata = getSeasonIndicators(alldata)



# set up confounders 
confs = getConfounders(alldata)
alldata = cbind(alldata, confs)

# save data with insample flag
outfile = paste0(dataDir, '/data-for-confounder-table-43777.csv')
write.table(alldata, outfile, row.names=FALSE, sep=',')




