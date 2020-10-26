

dataDir=Sys.getenv('PROJECT_DATA')


# read main phenotype data
data = read.table(paste0(dataDir, '/phenotypes/derived/analysis-dataset-43777.csv'), sep=',', header=TRUE)

# check for any duplicates
print(paste0('Num: ', length(data$eid)))
print(paste0('Num unique: ', length(unique(data$eid))))



# restrict to those who wore the accelerometer
i = which(!is.na(data$accelwearstartdate))
numRemoved = nrow(data) - length(i)
data = data[i,]
print(paste0('Num with no accel data: ', numRemoved))
print(paste0('Num: ', nrow(data)))




##
## remove participants with withdrawn consent

withdrawn = read.table(paste0(dataDir, '/participant-exclusions20200828.csv'), header=1)

print(head(withdrawn))
i = which(data$eid %in% withdrawn$pid)
print(length(i))
numRemoved = length(i)
if (numRemoved>0) {
	data = data[-i,]
}

print(paste0('Num participants withdrawn: ', numRemoved))
print(paste0('Num: ', nrow(data)))


##
## remove participants where accel has data problem

print(unique(data$acceldataprob))
print(length(which(data$acceldataprob==1)))
print(length(which(data$acceldataprob==2)))
print(length(which(data$acceldataprob==3)))



i = which(is.na(data$acceldataprob) | (data$acceldataprob == 0 ))
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with accel data problem: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants where accel could not be calibrated
        
i = which(data$accelgoodcal ==1)
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with accel, no calibration: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with less than 3 days wear time

i = which(data$accel3days==1)
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with < 3 days wear time: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with no BMI value

#i = which(!is.na(data$bmi))
#numRemoved = nrow(data) - length(i)
#data = data[i,]
	
#print(paste0('Num with no BMI: ', numRemoved))
#print(paste0('Num: ', nrow(data)))

##
## remove participants with no ethnicity value

i = which(!is.na(data$ethnicity) & data$ethnicity >= 0)
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no ethnicity: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with no sex value

i = which(!is.na(data$sex))
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no sex: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with no age value
        
i = which(!is.na(data$age))
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no age: ', numRemoved))
print(paste0('Num: ', nrow(data)))
	
##
## remove participants with no education value
        
ix = which(is.na(data$education0) | data$education0 == -3)

i = which(!is.na(data$education0) & (data$education0 >= 0 | data$education0 == -7) )
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no education: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with no townsend value
i = which(!is.na(data$townsend))
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no area-based socio-economic position: ', numRemoved))
print(paste0('Num: ', nrow(data)))


##
## remove participants with no household income value
i = which(!is.na(data$income) & data$income != -1 & data$income != -3)
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no household income: ', numRemoved))
print(paste0('Num: ', nrow(data)))

##
## remove participants with no smoking status
i = which(!is.na(data$smokestatus) & data$smokestatus != -3)
numRemoved = nrow(data) - length(i)
data = data[i,]

print(paste0('Num with no smokestatus: ', numRemoved))
print(paste0('Num: ', nrow(data)))


##
## remove participants with no number of cancers value

i = which(!is.na(data$numillnesscancer))
numRemoved = nrow(data) - length(i)
data = data[i,]
print(paste0('Num with no cancer illness number: ', numRemoved))
print(paste0('Num: ', nrow(data)))



write.table(data, paste0(dataDir, '/phenotypes/derived/analysis-dataset-subset-43777.csv'), sep=',', row.names=FALSE)


