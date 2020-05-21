
# version: CD or imp
# hybrid: TRUE:hybrid, FALSE: predicted categories
loadData <- function(version, hybrid=FALSE, daytimeonly=FALSE) {

	dataDir = Sys.getenv('PROJECT_DATA') 

	##
	## get our original sample for which we try to derive activity bouts data
	phenos = read.table(paste(dataDir, '/phenotypes/derived/analysis-dataset-subset-39441-39542.csv', sep=''), header=1, sep=',')

	print(dim(phenos))

	aggAll = loadBoutAggregations(dataDir, version, hybrid)

	print('Loaded bout aggregation data')
	print(dim(aggAll))



	##
	## merge pheno and activity data

	data = merge(phenos, aggAll, by.x='eid', by.y='id')

	print('Merged with pheno data')
	print(dim(data))


	## remove withdrawn

	withdrawn = read.table(paste(dataDir, '/participant-exclusions20200205.csv', sep=''), header=1, sep=',')
	ix = which(data$eid %in% withdrawn$pid)

	print(paste0('Number withdrawn removed: ', length(ix)))
	if (length(ix)>0) {
		data = data[-ix,]
	}
	print(dim(data))


	##
	## set num complete days variable
	print(version)
	if (version == 'imp') {
		data$num_days = data$num_imputed_days
		data$avm = data$imputedAVM

	}
	else if (version == 'CD') {
		data$num_days =	data$num_complete_days
		data$avm = data$completeDaysAVM

	}

	# remove complete days and imputed versions
	cxs = c('ClassMod', 'ClassSed', 'ClassSleep', 'ClassLight', 'ClassWalk', '100')
	for (cx in cxs) {
		data[, c(paste0('completeDays', cx))] = NULL
		data[, c(paste0('imputed', cx))] = NULL
	}


	## create sum across auc's within duration strata
#	for (duri in 1:3) {
#		boutOutcome = paste('dur', duri, '__', threshold, sep='')
#		data[,boutOutcome] = data[,paste('dur', duri, '_auc1__', threshold, sep='')] + data[,paste('dur', duri, '_auc2__', threshold, sep='')] + data[,paste('dur', duri, '_auc3__', threshold, sep='')]
#	}



	###
	### mortality variables


	##
	## check date death columns - there is one value in datadeath1

	# there is one value in datadeath1
	print(length(which(!is.null(data$datedeath1))))

	# check it is the same as in datedeath0
	ix = which(!is.null(data$datedeath1))
	print(paste0('one non null value in datedeath1 col is in datedeath0 col? ', data$datedeath1[ix] == data$datedeath0[ix]))


	###
        ### format dates

        library(data.table)
	
        data$datedeath0 = strptime(data$datedeath0, format='%Y-%m-%d')
	data$accelwearstartdate = strptime(data$accelwearstartdate, format='%Y-%m-%dT%H:%M:%S')

	print(head(data$yearbirth))
	data$yearbirth = strptime(paste0(data$yearbirth,'-07-01'), format='%Y-%m-%d')
	print(head(data$yearbirth))

	##
	## change death status for those who died after censor date
	
	## find those who died after the end of follow-up and change their datedeath to NA - i.e. they did not die during followup period.

	# scottish assessment centres (ids 11004 and 11005) - 30th November 2016 
	censorDateScot = strptime('2016-11-30', format='%Y-%m-%d')
	ixS = which((data$assesscentre %in% c(11004,11005)) & difftime(censorDateScot, data$datedeath0, units="days")<0)

	print(paste0("number of participants of scottish assessment centres who died after censor date: ", length(ixS)))
	#print(data[ixS,c('datedeath0', 'assesscentre')])
	data$datedeath0[ixS] = NA

	# english and welsh assessment centres - 31st January 2018 

	censorDateEngWel = strptime('2018-01-31', format='%Y-%m-%d')
        ixEW = which( (!(data$assesscentre %in% c(11004,11005))) & difftime(censorDateEngWel, data$datedeath0, units="days")<0)

	print(paste0("number of participants of english/welsh assessment centres who died after censor date: ", length(ixEW)))
	#print(data[ixEW,c('datedeath0', 'assesscentre')])
	data$datedeath0[ixEW] = NA


	##
	## set censoring dates

	data$censorDate = as.Date(NA)

	# censor date for Scotland
	ixSCens = which((data$assesscentre %in% c(11004,11005)))
	data$censorDate[ixSCens] = censorDateScot

	# censor date for England and Wales
	ixEWCens = which(!(data$assesscentre %in% c(11004,11005)))
	data$censorDate[ixEWCens] = censorDateEngWel


	##
	## create binary variable for death

	data$death = 0
	data$death[which(!is.na(data$datedeath0))] = 1

	print('death numbers:')
	print(length(which(data$death == 0)))
	print(length(which(data$death == 1)))


	##
	## create variables for mortality survival analysis

	data$accelwearenddate = data$accelwearstartdate + 7*24*60*60

	print(mean(data$accelwearstartdate))
	print(mean(data$accelwearenddate))

	# how many deaths in each month??

        ix = which(data$assesscentre == 11004 | data$assesscentre == 11005)

	data$deathMonth <- as.Date(cut(data$datedeath0, breaks = "month"))
#	print('monthly death summary for scotland assessment centres')
#	print(table(data$deathMonth[ix]))
	
#	print('monthly death summary for non-scotland assessment centres')
#	print(table(data$deathMonth[-ix]))



	##
	## survivalStatus - censoring variable, 1 = censored, 2=has died
	
	data$survivalStatus = NA
	data$survivalStatus[which(data$death==0)] = 1
	data$survivalStatus[which(data$death==1)] = 2



	##
	## survivalTime

	data$survivalTime = NA

	# index of participants who have died
	ix = which(!is.na(data$datedeath0))

	# survival time for those who have died
	data$survivalTime[ix] = difftime(data$datedeath0[ix], data$accelwearenddate[ix], units="days")/365.25

	# survivalTime for those still alive
	ixNoDeath = which(is.na(data$datedeath0))
	data$survivalTime[ixNoDeath] = difftime(data$censorDate[ixNoDeath], data$accelwearenddate[ixNoDeath], units="days")/365.25



	##
	## survival start age - age when accelerometer was first worn	

	data$survivalStartAge = difftime(data$accelwearenddate, data$yearbirth, units="days")/365.25
	

	##
	## survival end age - age when participant either died or is censored (age at 8th February 2018, last date of death in our sample)

	data$survivalEndAge = data$survivalStartAge + data$survivalTime
	

#	print(head(data[,c('death', 'survivalStatus', 'datedeath0', 'accelwearstartdate', 'accelwearenddate', 'yearbirth', 'survivalStartAge', 'survivalEndAge', 'survivalTime')]))

	return(data)

}



loadBoutAggregations <- function(dataDir, version, hybrid) {


	##
	## load aggregated activity bouts data

	i=1
	aggAll = NULL
	while (i <= 93001) {

		if (hybrid == TRUE) {
			aggfile = paste(dataDir, '/accel/derived/boutAggregationsWithN-hybrid-20190829/agg-',i,'-',version,'.csv', sep='')
		}
		else {
			aggfile = paste(dataDir, '/accel/derived/boutAggregationsWithN-predicted-20190829/agg-',i,'-',version,'.csv', sep='')
		}

		agg = read.table(aggfile, header=1, sep=',')
		if (is.null(aggAll)) {
			aggAll = agg
		} else {
			aggAll = rbind(aggAll, agg)
		}

		i=i+1000
	}


	data = aggAll


	if (hybrid==TRUE) {

		# the 'overall' variables are for the predicted version, so we calculate them for the hybrid approach using the strata variables

		data$overall_classLight = rowSums(data[,c('dur1_auc1__classLight', 'dur1_auc2__classLight', 'dur1_auc3__classLight', 'dur2_auc1__classLight','dur2_auc2__classLight', 'dur2_auc3__classLight', 'dur3_auc1__classLight', 'dur3_auc2__classLight', 'dur3_auc3__classLight')])
		data$overall_classSleep = rowSums(data[,c('dur1_auc1__classSleep', 'dur1_auc2__classSleep', 'dur1_auc3__classSleep', 'dur2_auc1__classSleep','dur2_auc2__classSleep', 'dur2_auc3__classSleep', 'dur3_auc1__classSleep', 'dur3_auc2__classSleep', 'dur3_auc3__classSleep')])
		data$overall_classSed = rowSums(data[,c('dur1_auc1__classSed', 'dur1_auc2__classSed', 'dur1_auc3__classSed', 'dur2_auc1__classSed', 'dur2_auc2__classSed', 'dur2_auc3__classSed', 'dur3_auc1__classSed', 'dur3_auc2__classSed', 'dur3_auc3__classSed')])		
		data$overall_100mg = rowSums(data[,c('dur1_auc1__100mg', 'dur1_auc2__100mg', 'dur1_auc3__100mg', 'dur2_auc1__100mg', 'dur2_auc2__100mg', 'dur2_auc3__100mg', 'dur3_auc1__100mg', 'dur3_auc2__100mg', 'dur3_auc3__100mg')])

	} else {

		if (version == 'imp') {

			data$overall_classMod = data$imputedClassMod
			data$overall_classSed = data$imputedClassSed
			data$overall_classSleep = data$imputedClassSleep
			data$overall_classLight = data$imputedClassLight
			data$overall_classWalk = data$imputedClassWalk

		} else if (version == 'CD') {

			data$overall_classMod = data$completeDaysClassMod
			data$overall_classSed = data$completeDaysClassSed
			data$overall_classSleep = data$completeDaysClassSleep
			data$overall_classLight = data$completeDaysClassLight
			data$overall_classWalk = data$completeDaysClassWalk

		}

	}



	return(data)

}
