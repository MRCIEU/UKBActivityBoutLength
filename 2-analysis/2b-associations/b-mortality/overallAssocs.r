

overallAssocs <- function(mydata, confs, confsAll, version, hybrid=FALSE) {


# overall associations - swapping

if (hybrid==TRUE) {
	resfile=paste0(Sys.getenv('RES_DIR'), '/results-overall-swapping-',version,'-hybrid.csv')
	classes = c('overall_classSleep', 'overall_classSed', 'overall_classLight', 'overall_100mg')
}
else {
	resfile=paste0(Sys.getenv('RES_DIR'), '/results-overall-swapping-',version,'-predicted.csv')
	classes = c('overall_classSleep', 'overall_classSed', 'overall_classWalk', 'overall_classLight', 'overall_classMod')
}

write('base,comp,test,estimate,beta,lower,upper,pvalue,schoenP,adjusted', file=resfile, append='FALSE')

for (i in 1:length(classes)) {
	print(i)
	compsx = classes
	compsx = compsx[-i]
	basex = classes[i]

	
	## unadjusted for confounders

	# swapping from	one class to another
	coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, mydata[,compsx], resfile, basex, 'NONE')

	# class vs all others: baseline is all other categories
	coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, mydata[,basex, drop=FALSE], resfile, 'all', 'NONE')


	

	## adjusted for confounders

	# swapping from one class to another
	coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, cbind(mydata[,compsx], confs), resfile, basex, 'SENS')

	# class vs all others: baseline is all other categories
	coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, cbind(mydata[,basex, drop=FALSE], confs), resfile, 'all', 'SENS')


	## adjusted for all (plus bmi and num illnesses)

        # swapping from one class to another
        coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, cbind(mydata[,compsx], confsAll), resfile, basex, 'ALL')

        # class vs all others: baseline is all other categories
        coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, cbind(mydata[,basex, drop=FALSE], confsAll), resfile, 'all', 'ALL')



}

# avm overall assoc
avm = mydata[,'avm', drop=FALSE]

# unadjusted
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, avm, resfile, 'NULL', 'NONE')

# adjusted
coxAssoc(mydata$survivalStartAge, mydata$survivalEndAge, mydata$survivalStatus, cbind(avm, confs), resfile, 'NULL', 'SENS')


}
