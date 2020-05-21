

# data.table for fread function
library(data.table)

dataDir=Sys.getenv('PROJECT_DATA')

pheno17810=Sys.getenv('UKB_DATA_PHENO')

# old basket
phenosold = fread(paste0(pheno17810,'/phenotypic/applications/17810/released/2019-12-05/data/data.39542.csv'), select=c('eid','31-0.0','34-0.0', '54-0.0','189-0.0','738-0.0','6138-0.0','6138-0.1','6138-0.2','6138-0.3','6138-0.4','6138-0.5','21000-0.0','21001-0.0','21003-0.0','40000-0.0','40000-1.0','90002-0.0','90010-0.0','90012-0.0','90015-0.0','90016-0.0', '110005-0.0', '110006-0.0', '6164-0.0', '6164-0.1','6164-0.2','6164-0.3','6164-0.4'), check.names=TRUE)

# latest basket but missing 40000 (date of death)
phenosnew = fread(paste0(pheno17810,'/phenotypic/applications/17810/released/2019-12-05/data/data.39441.csv'), select=c('eid','31-0.0','34-0.0', '54-0.0','189-0.0','738-0.0','6138-0.0','6138-0.1','6138-0.2','6138-0.3','6138-0.4','6138-0.5','21000-0.0','21001-0.0','21003-0.0','90002-0.0','90010-0.0','90012-0.0','90015-0.0','90016-0.0', '6164-0.0', '6164-0.1','6164-0.2','6164-0.3','6164-0.4', '20116-0.0', '134-0.0', '135-0.0'), check.names=TRUE)

phenos = merge(phenosold, phenosnew, by='eid', suffixes = c(".x",""))


print(dim(phenos))


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

phenos = phenos[, c('eid', 'X31.0.0', 'X34.0.0', 'X738.0.0', 'X21003.0.0', 'X54.0.0', 'X189.0.0', 'X6138.0.0', 'X6138.0.1', 'X6138.0.2', 'X6138.0.3', 'X6138.0.4', 'X6138.0.5', 'X21000.0.0', 'X21001.0.0', 'X90012.0.0', 'X90002.0.0', 'X90010.0.0', 'X90015.0.0', 'X90016.0.0', 'X40000.0.0', 'X40000.1.0', 'X110005.0.0', 'X110006.0.0', 'X6164.0.0', 'X6164.0.1','X6164.0.2','X6164.0.3','X6164.0.4', 'X20116.0.0', 'X134.0.0', 'X135.0.0')]

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


# 40000 - date of death
colnames(phenos)[which(colnames(phenos)=='X40000.0.0')] = 'datedeath0'
colnames(phenos)[which(colnames(phenos)=='X40000.1.0')] = 'datedeath1'



write.table(phenos, paste0(dataDir, '/phenotypes/derived/analysis-dataset-39441-39542.csv'), sep=',', row.names=FALSE)








