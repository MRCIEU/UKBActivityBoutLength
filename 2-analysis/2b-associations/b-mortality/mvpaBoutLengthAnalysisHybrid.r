


mvpaBoutLengthAnalysisHybrid <- function(mydata, confs, confsAll, version) {


# Version using 100mg threshold

resfile=paste0(Sys.getenv('RES_DIR'), '/results-MVPA-',version,'-hybrid.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')


###
### unadjusted

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100', FALSE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur1mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100', FALSE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur1mod100', 'dur2mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100', FALSE)


###
### adjusted

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100', TRUE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100', TRUE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100', TRUE)

###
### adjusted all (plus bmi and num illnesses)

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod100-S', TRUE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur3mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod100-S', TRUE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod100', 'dur2mod100', 'overall_classSleep', 'overall_classLight', 'overall_classSed')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod100-S', TRUE)

}
