


mvpaBoutLengthAnalysisHybrid <- function(mydata, confs, confsAll, version) {


# Version using 100mg threshold

resfile=paste0(Sys.getenv('RES_DIR'), '/results-MVPA-',version,'-hybrid.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')


###
### unadjusted

# baseline strata 1: leave out 1 to do swapping between strata 1 and others

covars = mydata[,c('dur2mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100', 'NONE')


# baseline strata 2: leave out 2 to do swapping between strata 2 and others

covars = mydata[,c('dur1mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100', 'NONE')


# baseline strata 3: leave out 3 to do swapping between strata 3 and others

covars = mydata[,c('dur1mod100', 'dur2mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100', 'NONE')


# baseline strata 4: leave out 4 to do swapping between strata 4 and others

covars = mydata[,c('dur1mod100', 'dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod100', 'NONE')


###
### adjusted


# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100', 'SENS')


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100', 'SENS')


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100', 'SENS')


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod100', 'SENS')


###
### adjusted all (plus bmi and num illnesses)


# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100', 'ALL')


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur3mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100', 'ALL')


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'dur4mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100', 'ALL')


# baseline strata 4: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod100', 'ALL')



}
