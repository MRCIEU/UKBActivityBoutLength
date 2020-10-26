
# version: CD or imp
# hybrid: TRUE:hybrid, FALSE: predicted categories
loadData <- function(version, hybrid=FALSE, daytimeonly=FALSE) {

	dataDir = Sys.getenv('PROJECT_DATA') 

	##
	## get our original sample for which we try to derive activity bouts data
	phenos = read.table(paste(dataDir, '/phenotypes/derived/analysis-dataset-subset-43777.csv', sep=''), header=1, sep=',')

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




	###
	### mortality variables

	data$accelwearstartdate = strptime(data$accelwearstartdate, format='%Y-%m-%d %H:%M:%S')
	data$accelwearenddate = data$accelwearstartdate + 7*24*60*60
	print(mean(data$accelwearstartdate))
        print(mean(data$accelwearenddate))	

	data$survivalStartDate = data$accelwearenddate

	data = createMortalityVariables(data)

#	print(summary(data$survivalStartAge))
#	print(sd(data$survivalStartAge))

#	print(summary(data$survivalTime))
#	print(sd(data$survivalTime))

	return(data)

}


createMortalityVariables <- function(data) {

	###
        ### format dates

        library(data.table)
	
        data$datedeath0 = strptime(data$datedeath0, format='%Y-%m-%d')

	print(head(data$yearbirth))
	data$yearbirth = strptime(paste0(data$yearbirth,'-07-01'), format='%Y-%m-%d')
	print(head(data$yearbirth))


	##
	## change death status for those who died after censor date
	
	## find those who died after the end of follow-up and change their datedeath to NA - i.e. they did not die during followup period.

	# censor date - 31st December 2019
	censorDate = strptime('2019-12-31', format='%Y-%m-%d')
	ixC = which(difftime(censorDate, data$datedeath0, units="days")<0)
	print(paste0("number of participants who died after censor date: ", length(ixC)))
	data$datedeath0[ixC] = NA


	##
	## create binary variable for death

	data$death = 0
	data$death[which(!is.na(data$datedeath0))] = 1

	print('death numbers:')
	print(length(which(data$death == 0)))
	print(length(which(data$death == 1)))


	##
	## create variables for mortality survival analysis


	# how many deaths in each month??

#        ix = which(data$assesscentre == 11004 | data$assesscentre == 11005)
#	data$deathMonth <- as.Date(cut(data$datedeath0, breaks = "month"))
#	print('monthly death summary for scotland assessment centres')
#	print(table(data$deathMonth[ix]))	
#	print('monthly death summary for non-scotland assessment centres')
#	print(table(data$deathMonth[-ix]))



	##
	## survivalStatus - censoring variable, 1 = censored, 2=has died
	
	data$survivalStatus = NA
	data$survivalStatus[which(data$death==0)] = 1
	data$survivalStatus[which(data$death==1)] = 2


	print('a')
	##
	## survivalTime

	data$survivalTime = NA

	# survival time for those who have died
	ix = which(!is.na(data$datedeath0))
	print('b')
	data$survivalTime[ix] = difftime(data$datedeath0[ix], data$survivalStartDate[ix], units="days")/365.25
	print('c')

	# survivalTime for those still alive
	ixNoDeath = which(is.na(data$datedeath0))
	data$survivalTime[ixNoDeath] = difftime(censorDate, data$survivalStartDate[ixNoDeath], units="days")/365.25

	print('d')
	print(head(data$survivalTime))
	data$survivalTime = as.numeric(data$survivalTime)
	print('e')

	##
	## survival start age - age when accelerometer was first worn	

	data$survivalStartAge = difftime(data$survivalStartDate, data$yearbirth, units="days")/365.25
	
	print('aaaaa')
	print(class(data$survivalStartAge))
	data$survivalStartAge = as.numeric(data$survivalStartAge)

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
			aggfile = paste(dataDir, '/accel/derived/boutAggregationsWithN-hybrid-20200903/agg-',i,'-',version,'.csv', sep='')
		}
		else {
			aggfile = paste(dataDir, '/accel/derived/boutAggregationsWithN-predicted-20200903/agg-',i,'-',version,'.csv', sep='')
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


#	print(head(data))

	if (hybrid==TRUE) {

		# the 'overall' variables are for the predicted version, so we calculate them for the hybrid approach using the strata variables

		data$overall_classLight = rowSums(data[,c('dur1__classLight', 'dur2__classLight', 'dur3__classLight', 'dur4__classLight')])
		data$overall_classSleep = rowSums(data[,c('dur1__classSleep', 'dur2__classSleep', 'dur3__classSleep', 'dur4__classSleep')])
		data$overall_classSed = rowSums(data[,c('dur1__classSed', 'dur2__classSed', 'dur3__classSed', 'dur4__classSed')])
		data$overall_100mg = rowSums(data[,c('dur1__100mg', 'dur2__100mg', 'dur3__100mg', 'dur4__100mg')])

		colnames(data)[which(colnames(data)=="dur1__100mg")] = "dur1mod100"
		colnames(data)[which(colnames(data)=="dur2__100mg")] = "dur2mod100"
		colnames(data)[which(colnames(data)=="dur3__100mg")] = "dur3mod100"
		colnames(data)[which(colnames(data)=="dur4__100mg")] = "dur4mod100"

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

		colnames(data)[which(colnames(data)=="dur1__classMod")] = "dur1mod"
		colnames(data)[which(colnames(data)=="dur2__classMod")] = "dur2mod"
		colnames(data)[which(colnames(data)=="dur3__classMod")] = "dur3mod"
		colnames(data)[which(colnames(data)=="dur4__classMod")] = "dur4mod"


	}


	colnames(data)[which(colnames(data)=="dur1__classSed")] = "dur1sed"
	colnames(data)[which(colnames(data)=="dur2__classSed")] = "dur2sed"
	colnames(data)[which(colnames(data)=="dur3__classSed")] = "dur3sed"
	colnames(data)[which(colnames(data)=="dur4__classSed")] = "dur4sed"

	data$dur1sed = data$dur1sed + data$dur2sed
	data$dur2sed = data$dur3sed
	data$dur3sed = data$dur4sed


	return(data)

}
