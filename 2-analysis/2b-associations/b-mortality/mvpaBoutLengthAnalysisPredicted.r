


mvpaBoutLengthAnalysisPredicted <- function(mydata, confs, confsAll, version) {

resfile=paste0(Sys.getenv('RES_DIR'), '/results-MVPA-',version,'-predicted.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')




# Version using predicted MVPA


###
### unadjusted

# baseline strata 1: leave out 1 to do swapping between strata 1 and others

covars = mydata[,c('dur2mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod', 'NONE')


# baseline strata 2: leave out 2 to do swapping between strata 2 and others

covars = mydata[,c('dur1mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod', 'NONE')


# baseline strata 3: leave out 3 to do swapping between strata 3 and others

covars = mydata[,c('dur1mod', 'dur2mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod', 'NONE')

# baseline strata 4: leave out 4 to do swapping between strata 4 and others

covars = mydata[,c('dur1mod', 'dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod', 'NONE')



###
### adjusted

# baseline strata 1: leave out 1 to do swapping between strata 1 and others

covars = cbind(mydata[,c('dur2mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod', 'SENS')


# baseline strata 2: leave out 2 to do swapping between strata 2 and others

covars = cbind(mydata[,c('dur1mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod', 'SENS')


# baseline strata 3: leave out 3 to do swapping between strata 3 and others

covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod', 'SENS')

# baseline strata 4: leave out 4 to do swapping between strata 4 and others


covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod', 'SENS')


###
### adjusted all (plus bmi and num illnesses)

# baseline strata 1: leave out 1 to do swapping between strata 1 and others

covars = cbind(mydata[,c('dur2mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod', 'ALL')


# baseline strata 2: leave out 2 to do swapping between strata 2 and others

covars = cbind(mydata[,c('dur1mod', 'dur3mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod', 'ALL')


# baseline strata 3: leave out 3 to do swapping between strata 3 and others

covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'dur4mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod', 'ALL')


# baseline strata 4: leave out 4 to do swapping between strata 4 and others

covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur4mod', 'ALL')


}
