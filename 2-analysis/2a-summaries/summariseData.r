##
## supplementary table 2 - summarising median and IQR of time spent in activity categories on average per day.

source('../loadData.r')

 
summariseData <- function(version, hybrid=FALSE) {

	print('----------------')
	print(paste0(version, ', ', hybrid))

	datax = loadData(version, hybrid)

	datax$dur1sed = datax$dur1_auc1__classSed + datax$dur1_auc2__classSed + datax$dur1_auc3__classSed
	datax$dur2sed = datax$dur2_auc1__classSed + datax$dur2_auc2__classSed + datax$dur2_auc3__classSed
	datax$dur3sed = datax$dur3_auc1__classSed + datax$dur3_auc2__classSed + datax$dur3_auc3__classSed

	# convert to average per day
	datax$dur1sed = datax$dur1sed/datax$num_days
	datax$dur2sed = datax$dur2sed/datax$num_days
	datax$dur3sed = datax$dur3sed/datax$num_days

	print('classsed1')
	q=quantile(datax$dur1sed, probs=c(0.25, 0.5, 0.75))
	print(q)

	print('classsed2')
	q=quantile(datax$dur2sed, probs=c(0.25, 0.5, 0.75))
	print(q)

	print('classsed3')
	q=quantile(datax$dur3sed, probs=c(0.25, 0.5, 0.75))
	print(q)

	print('classsleep')
	q=quantile(datax$overall_classSleep/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)

	print('classsed')
	q=quantile(datax$overall_classSed/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)

	print('classlight')
	q=quantile(datax$overall_classLight/datax$num_days, probs=c(0.25, 0.5, 0.75))
	print(q)

	if (hybrid == FALSE) {

		print('classwalk')
		q=quantile(datax$overall_classWalk/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)

		print('classmod')
		q=quantile(datax$overall_classMod/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)

		datax$dur1mod = datax$dur1_auc1__classMod + datax$dur1_auc2__classMod + datax$dur1_auc3__classMod
		datax$dur2mod = datax$dur2_auc1__classMod + datax$dur2_auc2__classMod + datax$dur2_auc3__classMod
		datax$dur3mod = datax$dur3_auc1__classMod + datax$dur3_auc2__classMod + datax$dur3_auc3__classMod

		# convert to average per day
		datax$dur1mod = datax$dur1mod/datax$num_days
		datax$dur2mod = datax$dur2mod/datax$num_days
		datax$dur3mod = datax$dur3mod/datax$num_days
		
		print('classmod 1')
		q=quantile(datax$dur1mod, probs=c(0.25, 0.5, 0.75))
		print(q)

		print('classmod 2')
		q=quantile(datax$dur2mod, probs=c(0.25, 0.5, 0.75))
		print(q)

		print('classmod 3')
		q=quantile(datax$dur3mod, probs=c(0.25, 0.5, 0.75))
		print(q)
	}
	else {
		print('100mg')
		q=quantile(datax$overall_100mg/datax$num_days, probs=c(0.25, 0.5, 0.75))
		print(q)

		datax$dur1mod100 = datax$dur1_auc1__100mg + datax$dur1_auc2__100mg + datax$dur1_auc3__100mg
		datax$dur2mod100 = datax$dur2_auc1__100mg + datax$dur2_auc2__100mg + datax$dur2_auc3__100mg
		datax$dur3mod100 = datax$dur3_auc1__100mg + datax$dur3_auc2__100mg + datax$dur3_auc3__100mg
		datax$dur1mod100 = datax$dur1mod100/datax$num_days
		datax$dur2mod100 = datax$dur2mod100/datax$num_days
		datax$dur3mod100 = datax$dur3mod100/datax$num_days

		print('100mg 1')
                q=quantile(datax$dur1mod100, probs=c(0.25, 0.5, 0.75))
                print(q)
		
		print('100mg 2')
		q=quantile(datax$dur2mod100, probs=c(0.25, 0.5, 0.75))
		print(q)

		print('100mg 3')
		q=quantile(datax$dur3mod100, probs=c(0.25, 0.5, 0.75))
		print(q)

	}

}

summariseData('CD')
summariseData('CD', TRUE)



summariseData('imp')
summariseData('imp', TRUE)

