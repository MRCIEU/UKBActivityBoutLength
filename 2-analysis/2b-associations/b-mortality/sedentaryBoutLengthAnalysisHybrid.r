

sedentaryBoutLengthAnalysisHybrid <- function(mydata, confs, confsAll, version) {

resfile=paste0(Sys.getenv('RES_DIR'), '/results-SEDENTARY-',version,'-hybrid.csv')
write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')



#######
# from non-sedentary state to one of the sedentary bout lengths

###
### unadjusted

covars = mydata[,c('dur1sed', 'dur2sed', 'dur3sed')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed', 'NONE')

###
### adjusted

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'dur3sed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed', 'SENS')

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur1sed + mydata$dur2sed + mydata$dur3sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)

###
### adjusted sensitivity

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'dur3sed')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed', 'ALL')



######
# from other state (maybe sedentary) to a sedentary bout length stratum


### comparison: dur1sed

###
### unadjusted


covars = mydata[,'dur1sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1', 'NONE')

###
### adjusted

covars = cbind(mydata[,'dur1sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1', 'SENS')

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur1sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)


###
### adjusted sensitivity

covars = cbind(mydata[,'dur1sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed1', 'ALL')


### comparison:	dur2sed

###
### unadjusted


covars = mydata[,'dur2sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2', 'NONE')

###
### adjusted

covars = cbind(mydata[,'dur2sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2', 'SENS')

#res.cox <- coxph(Surv(, mydata$survivalStatus) ~ mydata$dur2sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)

###
### adjusted sensitivity

covars = cbind(mydata[,'dur2sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed2', 'ALL')



### comparison:	dur3sed

###
### unadjusted


covars = mydata[,'dur3sed', drop=FALSE]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3', 'NONE')

###
### adjusted

covars = cbind(mydata[,'dur3sed', drop=FALSE], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3', 'SENS')

#res.cox <- coxph(Surv(mydata$survivalTime, mydata$survivalStatus) ~ mydata$dur3sed + . ,data=confs)
#sumx = summary(res.cox)
#print(sumx)

###
### adjusted sensitivity

covars = cbind(mydata[,'dur3sed', drop=FALSE], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'not-sed3', 'ALL')




#resfile=paste0(Sys.getenv('RES_DIR'), '/results-SEDENTARY-100mg-',version,'.csv')
#write('base,comp,test,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')


### test only adjusting for sedentary 

covars = cbind(mydata[,c('dur2sed', 'dur3sed', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed-sedadj', 'SENS')

covars = cbind(mydata[,c('dur1sed', 'dur3sed', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed-sedadj', 'SENS')

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'overall_classSed')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed-sedadj', 'SENS')





###
### unadjusted


# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed', 'NONE')


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed', 'NONE')


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')]
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed', 'NONE')

###
### adjusted

# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = cbind(mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed', 'SENS')


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed', 'SENS')


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confs)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed', 'SENS')


###
### adjusted all (plus bmi and num illnesses)

# baseline strata 1: leave out 1 to do swapping between short sedentary bouts and other states

covars = cbind(mydata[,c('dur2sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur1sed', 'ALL')


# baseline strata 2: leave out 2 to do swapping between medium sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur3sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur2sed', 'ALL')


# baseline strata 3: leave out 3 to do swapping between long sedentary bouts and other states

covars = cbind(mydata[,c('dur1sed', 'dur2sed', 'overall_classSleep', 'overall_classLight', 'overall_100mg')], confsAll)
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, covars, resfile, 'dur3sed', 'ALL')



}
