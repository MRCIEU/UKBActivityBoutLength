
# version: CD or imp
# hybrid: TRUE:hybrid, FALSE: predicted categories
loadDataFromSaved <- function(version, hybrid=FALSE) {

	dataDir = Sys.getenv('PROJECT_DATA') 

	source('loadData.r')

	##
	## get our original sample for which we try to derive activity bouts data
	phenos = read.table(paste(dataDir, '/phenotypes/derived/analysis-dataset-subset-43777.csv', sep=''), header=1, sep=',')

	print(dim(phenos))

	#aggAll = loadBoutAggregations(dataDir, version, hybrid)
        aggAll = read.table(paste(dataDir, '/phenotypes/derived/activity_bouts/bouts-',version,'-hybrid.csv', sep=''), header=1, sep=',')

	print('Loaded bout aggregation data')
	print(dim(aggAll))



	##
	## merge pheno and activity data

	data = merge(phenos, aggAll, by='eid')

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


	return(data)

}


