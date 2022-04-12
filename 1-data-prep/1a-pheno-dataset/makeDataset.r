

# data.table for fread function
library(data.table)

dataDir=Sys.getenv('PROJECT_DATA')

pheno17810=Sys.getenv('UKB_DATA_PHENO')


phenos = fread(paste0(pheno17810,'/phenotypic/applications/17810/dev/release_candidate/data/data.43777.csv'), check.names=TRUE)
phenos = as.data.frame(phenos)

print(dim(phenos))



pheno110005 = fread(paste0(pheno17810,'/phenotypic/applications/17810/released/2019-12-05/data/data.39542.csv'), check.names=TRUE, select=c('eid', '110005-0.0'))
phenos = merge(phenos, pheno110005, by='eid', all.x=TRUE, all.y=FALSE, sort=FALSE)
print(dim(phenos))



## merge death


print(dim(phenos))


death = fread(paste0(pheno17810,'/phenotypic/applications/17810/dev/release_candidate/data/death-20200928.txt'), select=c('eid', 'date_of_death'));

# remove duplicate rows in death data frame
death = unique(death)

# keep all in phenos and add death column to it
phenos = merge(phenos, death, by='eid', all.x=TRUE, all.y=FALSE, sort=FALSE)

#print(dim(phenos))


### check old death data is a subset of new death data
#
phenos$date_of_death = as.Date(phenos$date_of_death, format='%d/%m/%Y')
phenos$X40000.0.0 = as.Date(phenos$X40000.0.0, format='%Y-%m-%d')
phenos$X40000.1.0 = as.Date(phenos$X40000.1.0, format='%Y-%m-%d')

# compare old death dates with new
print('num old death dates not matching new death data')
ix = which(!is.na(phenos$X40000.0.0) & phenos$date_of_death !=phenos$X40000.0.0)
print(length(ix))
###### two differences, of 1 and 8 days
#
ix = which(!is.na(phenos$X40000.1.0) & phenos$date_of_death !=phenos$X40000.1.0)
print(length(ix))
###### no differences for second death instance

# count number of new ones
print('num new death dates not in old data')
ix = which(is.na(phenos$X40000.0.0) & is.na(phenos$X40000.1.0) & !is.na(phenos$date_of_death))
print(length(ix))






###
### covariates

# sex 31
# age when attending assessment centre 21003
# assessment centre 54
# Townsend deprivation index 189
# Qualifications 6138
# ethnicity 21000
# BMI 21001

###
### accel variables

# accel avg 90012
# data problem indicator 90002
# data quality good calibration 90016
# at least 3 valid days 90015
# date of wear time start 90010

## keep only variables we need


colnames(phenos)[which(colnames(phenos)=='X31.0.0')] = 'sex'
colnames(phenos)[which(colnames(phenos)=='X34.0.0')] = 'yearbirth'
colnames(phenos)[which(colnames(phenos)=='X21003.0.0')] = 'age'


colnames(phenos)[which(colnames(phenos)=='X54.0.0')] = 'assesscentre'
colnames(phenos)[which(colnames(phenos)=='X189.0.0')] = 'townsend'
colnames(phenos)[which(colnames(phenos)=='X738.0.0')] = 'income'
colnames(phenos)[which(colnames(phenos)=='X6138.0.0')] = 'education0'
colnames(phenos)[which(colnames(phenos)=='X6138.0.1')] = 'education1'
colnames(phenos)[which(colnames(phenos)=='X6138.0.2')] = 'education2'
colnames(phenos)[which(colnames(phenos)=='X6138.0.3')] = 'education3'
colnames(phenos)[which(colnames(phenos)=='X6138.0.4')] = 'education4'
colnames(phenos)[which(colnames(phenos)=='X6138.0.5')] = 'education5'
colnames(phenos)[which(colnames(phenos)=='X21000.0.0')] = 'ethnicity'
colnames(phenos)[which(colnames(phenos)=='X21001.0.0')] = 'bmi'
colnames(phenos)[which(colnames(phenos)=='X90012.0.0')] = 'accelavg'
colnames(phenos)[which(colnames(phenos)=='X90002.0.0')] = 'acceldataprob'
colnames(phenos)[which(colnames(phenos)=='X90010.0.0')] = 'accelwearstartdate'
colnames(phenos)[which(colnames(phenos)=='X90015.0.0')] = 'accel3days'
colnames(phenos)[which(colnames(phenos)=='X90016.0.0')] = 'accelgoodcal'
colnames(phenos)[which(colnames(phenos)=='X110005.0.0')] = 'accelaccept'
colnames(phenos)[which(colnames(phenos)=='X110006.0.0')] = 'accelinvitedate'
colnames(phenos)[which(colnames(phenos)=='X135.0.0')] = 'numillness'
colnames(phenos)[which(colnames(phenos)=='X134.0.0')] = 'numillnesscancer'
colnames(phenos)[which(colnames(phenos)=='X20116.0.0')] = 'smokestatus'
#colnames(phenos)[which(colnames(phenos)=='X40000.0.0')] = 'datedeath0'
colnames(phenos)[which(colnames(phenos)=='date_of_death')] = 'datedeath0'

print('accel wear start to date')
print(head(phenos$accelwearstartdate))

phenos$accelwearstartdateX = as.POSIXct(strptime(phenos$accelwearstartdate, format='%Y-%m-%d %H:%M:%S'), tz='UTC')

print(head(phenos$accelwearstartdate))


##
## identify those alread with circ system disease before accel wear

print('make circulatory disease var')


phenos$prev_firstocc_circ = 0
circPhenos = c("X131270.0.0","X131272.0.0","X131274.0.0","X131276.0.0","X131278.0.0","X131280.0.0","X131282.0.0","X131284.0.0","X131286.0.0","X131288.0.0","X131290.0.0","X131292.0.0","X131294.0.0","X131296.0.0","X131298.0.0","X131300.0.0","X131302.0.0","X131304.0.0","X131306.0.0","X131308.0.0","X131310.0.0","X131312.0.0","X131314.0.0","X131316.0.0","X131318.0.0","X131320.0.0","X131322.0.0","X131324.0.0","X131326.0.0","X131328.0.0","X131330.0.0","X131332.0.0","X131334.0.0","X131336.0.0","X131338.0.0","X131340.0.0","X131342.0.0","X131344.0.0","X131346.0.0","X131348.0.0","X131350.0.0","X131352.0.0","X131354.0.0","X131356.0.0","X131358.0.0","X131360.0.0","X131362.0.0","X131364.0.0","X131366.0.0","X131368.0.0","X131370.0.0","X131372.0.0","X131374.0.0","X131376.0.0","X131378.0.0","X131380.0.0","X131382.0.0","X131384.0.0","X131386.0.0","X131388.0.0","X131390.0.0","X131392.0.0","X131394.0.0","X131396.0.0","X131398.0.0","X131400.0.0","X131402.0.0","X131404.0.0","X131406.0.0","X131408.0.0","X131410.0.0","X131412.0.0","X131414.0.0","X131416.0.0","X131418.0.0","X131420.0.0","X131422.0.0")
for (i in 1: length(circPhenos)) { 
	nx = circPhenos[i]
	print(nx)
	x = as.POSIXct(strptime(phenos[,nx], format='%Y-%m-%d')) 
	ixCirc = which(difftime(x,phenos$accelwearstartdate)<0)
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
	ixResp = which(difftime(x,phenos$accelwearstartdate)<0)
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
	ixCanc = which(difftime(x,phenos$accelwearstartdate)<0)
	phenos$prev_firstocc_canc[ixCanc] = 1
}

print(summary(phenos$prev_firstocc_canc))


print(colnames(phenos))


# save all also so we can generate the summary table with disease incidence variables for those not in sample
write.table(phenos, paste0(dataDir, '/phenotypes/derived/analysis-dataset-43777-all.csv'), sep=',', row.names=FALSE)


## keep only the variables we need and save them

phenos = phenos[,c('eid','sex', 'yearbirth', 'age', 'assesscentre', 'townsend', 'income', 'education0', 'education1', 'education2', 'education3', 'education4', 'education5', 'ethnicity', 'bmi', 'accelavg', 'accelaccept', 'acceldataprob', 'accelwearstartdate', 'accel3days' ,'accelgoodcal', 'numillness', 'numillnesscancer', 'smokestatus', 'datedeath0', 'prev_firstocc_circ', 'prev_firstocc_resp', 'prev_firstocc_canc')]

write.table(phenos, paste0(dataDir, '/phenotypes/derived/analysis-dataset-43777.csv'), sep=',', row.names=FALSE)








