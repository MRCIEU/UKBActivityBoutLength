


mvpaBoutLengthAnalysisPredicted <- function(mydata, confs, confsAll, version) {

resfile=paste0(Sys.getenv('RES_DIR'), '/results-MVPA-',version,'-predicted.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')




# Version using predicted MVPA


###
### unadjusted

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod', FALSE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur1mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod', FALSE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = mydata[,c('dur1mod', 'dur2mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod', FALSE)


###
### adjusted

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod', TRUE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod', TRUE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod', TRUE)



###
### adjusted all (plus bmi and num illnesses)

# baseline strata 1: leave out 1 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur2mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1mod-S', TRUE)


# baseline strata 2: leave out 2 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod', 'dur3mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2mod-S', TRUE)


# baseline strata 3: leave out 3 to do swapping between short mvpa bouts and other states

covars = cbind(mydata[,c('dur1mod', 'dur2mod', 'overall_classSleep', 'overall_classWalk', 'overall_classSed', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3mod-S', TRUE)





}
