##
## supplementary table 2 - summarising median and IQR of time spent in activity categories on average per day.

source('../loadData.r')

 
summariseData <- function(version, hybrid=FALSE) {

	print('----------------')
	print(paste0(version, ', ', hybrid))

	datax = loadData(version, hybrid)


	# convert to average per day
	datax$dur1sed = datax$dur1sed/datax$num_days
	datax$dur2sed = datax$dur2sed/datax$num_days
	datax$dur3sed = datax$dur3sed/datax$num_days

	print('classsed1')
	q=quantile(datax$dur1sed, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$dur1sed != 0))))

	print('classsed2')
	q=quantile(datax$dur2sed, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$dur2sed != 0))))

	print('classsed3')
	q=quantile(datax$dur3sed, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$dur3sed != 0))))

	print('classsleep')
	q=quantile(datax$overall_classSleep/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$overall_classSleep != 0))))

	print('classsed')
	q=quantile(datax$overall_classSed/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$overall_classSed != 0))))

	print('classlight')
	q=quantile(datax$overall_classLight/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)
	print(paste0('num zeros', length(which(datax$overall_classLight != 0))))

	if (hybrid == FALSE) {

		print('classwalk')
		q=quantile(datax$overall_classWalk/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$overall_classWalk != 0))))

		print('classmod')
		q=quantile(datax$overall_classMod/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$overall_classMod != 0))))

		# convert to average per day
		datax$dur1mod = datax$dur1mod/datax$num_days
		datax$dur2mod = datax$dur2mod/datax$num_days
		datax$dur3mod = datax$dur3mod/datax$num_days
		datax$dur4mod = datax$dur4mod/datax$num_days

		print('classmod 1')
		q=quantile(datax$dur1mod, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$dur1mod != 0))))

		print('classmod 2')
		q=quantile(datax$dur2mod, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$dur2mod != 0))))

		print('classmod 3')
		q=quantile(datax$dur3mod, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$dur3mod != 0))))

		print('classmod 4')
                q=quantile(datax$dur4mod, probs=c(0.25, 0.5, 0.75))
                print(q)
		print(paste0('num zeros', length(which(datax$dur4mod != 0))))

	}
	else {
		print('100mg')
		q=quantile(datax$overall_100mg/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$overall_100mg != 0))))

		datax$dur1mod100 = datax$dur1mod100/datax$num_days
		datax$dur2mod100 = datax$dur2mod100/datax$num_days
		datax$dur3mod100 = datax$dur3mod100/datax$num_days
		datax$dur4mod100 = datax$dur4mod100/datax$num_days

		print('100mg 1')
                q=quantile(datax$dur1mod100, probs=c(0.25, 0.5, 0.75))
                print(q)
		print(paste0('num zeros', length(which(datax$dur1mod100 != 0))))
		
		print('100mg 2')
		q=quantile(datax$dur2mod100, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$dur2mod100 != 0))))

		print('100mg 3')
		q=quantile(datax$dur3mod100, probs=c(0.25, 0.5, 0.75))
		print(q)
		print(paste0('num zeros', length(which(datax$dur3mod100 != 0))))

		print('100mg 4')
                q=quantile(datax$dur4mod100, probs=c(0.25, 0.5, 0.75))
                print(q)
		print(paste0('num zeros', length(which(datax$dur4mod100 != 0))))

	}

}

summariseData('CD')
summariseData('CD', TRUE)



summariseData('imp')
summariseData('imp', TRUE)

