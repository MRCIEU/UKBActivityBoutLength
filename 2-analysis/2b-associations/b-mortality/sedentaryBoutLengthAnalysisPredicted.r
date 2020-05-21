

sedentaryBoutLengthAnalysisPredicted <- function(mydata, confs, confsAll, version) {

resfile=paste0(Sys.getenv('RES_DIR'), '/results-SEDENTARY-',version,'-predicted.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')



#######
# from non-sedentary state to one of the sedentary bout lengths

###
### unadjusted

covars = mydata[,c('dur1sed', 'dur2sed', 'dur3sed')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed', FALSE)

###
### adjusted

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'dur3sed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed', TRUE)

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur1sed + mydata$dur2sed + mydata$dur3sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)

###
### adjusted sensitivity

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'dur3sed')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed-S', TRUE)



######
# from other state (maybe sedentary) to a sedentary bout length stratum


### comparison: dur1sed

###
### unadjusted


covars = mydata[,'dur1sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1', FALSE)

###
### adjusted

covars = cbind(mydata[,'dur1sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1', TRUE)

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur1sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)


###
### adjusted sensitivity

covars = cbind(mydata[,'dur1sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1-S', TRUE)



### comparison:	dur2sed

###
### unadjusted


covars = mydata[,'dur2sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2', FALSE)

###
### adjusted

covars = cbind(mydata[,'dur2sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2', TRUE)

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur2sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)


###
### adjusted sensitivity

covars = cbind(mydata[,'dur2sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2-S', TRUE)



### comparison:	dur3sed

###
### unadjusted


covars = mydata[,'dur3sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3', FALSE)

###
### adjusted

covars = cbind(mydata[,'dur3sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3', TRUE)

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur3sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)

###
### adjusted sensitivity

covars = cbind(mydata[,'dur3sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3-S', TRUE)





######
# my bout analyses


###
### unadjusted

# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed', FALSE)


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed', FALSE)


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed', FALSE)



###
### adjusted

# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = cbind(mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed', TRUE)


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed', TRUE)


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed', TRUE)

###
### adjusted all (plus bmi and num illnesses)

# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = cbind(mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed-S', TRUE)


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed-S', TRUE)


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classWalk', 'overall_classMod', 'overall_classLight')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed-S', TRUE)



}
