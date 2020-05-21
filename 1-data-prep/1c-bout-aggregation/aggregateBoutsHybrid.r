

source('exclude100mg.r')

## version is either CD or imp
aggregateBoutsHybrid <- function(version, i, daytimeonly=FALSE) {


derivedDataDir=paste(Sys.getenv('HOME'), '/2016-biobank-accelerometer/data/accel/derived/', sep='')
boutDir=paste(derivedDataDir, 'activityBouts/', sep='')
aggDir=paste(derivedDataDir, 'boutAggregationsWithN-hybrid-20190829/', sep='')


	print(i)

	# clear aggregation
	if (daytimeonly==TRUE) {
		aggFile = paste(aggDir, 'agg-', i, '-', version, 'DAY.csv', sep='')
		cat(NULL,file=aggFile, sep='')
	} else {
		aggFile = paste(aggDir, 'agg-', i, '-', version, '.csv', sep='')
                cat(NULL,file=aggFile, sep='')
	}

	subdir=paste(boutDir, 'boutssample', i, '/', sep='')


	## check directory exists
	if (!file.exists(subdir)) {

		print(paste('Directory does not exist:', subdir, sep=''))
	}
	else {

	# get id list for this part
	if (version == "CD") {
		idlistinc = read.table(paste(subdir, 'cd-ids-inc.txt', sep=''), header=0, sep=',')
	}
	else if (version == "imp") {
		idlistinc = read.table(paste(subdir, 'imp-ids-inc.txt', sep=''), header=0, sep=',')
	}

	# get activity summary for this part
	activitySummary = read.table(paste(subdir, 'activity-summary.txt', sep=''), header=1, sep=',')
#	print(paste(subdir, 'activity-summary.txt', sep=''))

	##### is this needed?
	activitySummary$num_imputed_days = as.integer(activitySummary$num_imputed_days)
	activitySummary$num_complete_days = as.integer(activitySummary$num_complete_days)


	## load each persons bouts and aggregate them
	for (ix in 1:nrow(idlistinc)) {

		id = idlistinc$V1[ix]

		# get bouts for this person
                bouts100 = read.table(paste(subdir, 'bouts100-',version,'-', id, '.csv', sep=''), header=0, sep=',', col.names=c('boutidx', 'duration_secs', 'auc', 'complexity', 'valid', 'partial', 'startdate', 'enddate', 'boutContainsImputeBound', 'edgeImputeBound'))
                bouts100$duration = bouts100$duration_secs / 60
                bouts100$auc = bouts100$auc * 60

		bouts100$startdate = as.POSIXlt(bouts100$startdate)
		bouts100$enddate = as.POSIXlt(bouts100$enddate)


		###
                ### class prediction

		# get bouts for this person
		bouts = read.table(paste(subdir, 'boutsClass-',version,'-', id, '.csv', sep=''), header=0, sep=',', col.names=c('boutidx', 'duration_secs', 'auc', 'complexity', 'class', 'valid', 'partial', 'startdate', 'enddate', 'boutContainsImputeBound', 'edgeImputeBound'))

		bouts$startdate = as.POSIXlt(bouts$startdate)
		bouts$enddate	= as.POSIXlt(bouts$enddate)

                bouts$duration = bouts$duration_secs / 60
                bouts$auc = bouts$auc * 60


		# aggregate bouts sedentary
		print('sed')
		boutsSed = bouts[which(bouts$class == 'sedentary'),]

		boutsSed = exclude100mg(boutsSed, bouts100)
                boutaggSed = aggregateBoutsForPerson(boutsSed, daytimeonly)
                colnames(boutaggSed) <- paste(colnames(boutaggSed), '_classSed', sep = "_")

		# aggregate bouts tasks.light
		print('light')
		boutsLight = bouts[which(bouts$class == 'tasks.light' | bouts$class == 'moderate' | bouts$class == 'walking'),]
		boutsLight = exclude100mg(boutsLight, bouts100)

		# because we combine the remaining walking, moderate and light bouts together, we first merge those that are adjacent so they count as a single bout
		boutsLight = mergeAdjacent(boutsLight)


                boutaggLight = aggregateBoutsForPerson(boutsLight, daytimeonly)
                colnames(boutaggLight) <- paste(colnames(boutaggLight), '_classLight', sep = "_")

		# aggregate bouts sleep
		print('sleep')
		boutsSleep = bouts[which(bouts$class == 'sleep'),]
		boutsSleep = exclude100mg(boutsSleep, bouts100)
                boutaggSleep = aggregateBoutsForPerson(boutsSleep, daytimeonly)
                colnames(boutaggSleep) <- paste(colnames(boutaggSleep), '_classSleep', sep = "_")


		###
                ### aggregate 100mg threshold bouts

		print('100mg')
		boutagg100 = aggregateBoutsForPerson(bouts100, daytimeonly)
		colnames(boutagg100) <- paste(colnames(boutagg100), '_100mg', sep = "_")



		###
		### combine data together

		boutagg = cbind.data.frame(id, boutagg100, boutaggSleep, boutaggSed, boutaggLight, activitySummary[which(activitySummary$id == id),])

		# write aggregation to file
		if (ix==1) {
			print('first')
			filex = file(aggFile, open='wt')
			write.table(boutagg, file=filex,  sep=',', col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
			close(filex)
		}
		else {
			filex =	file(aggFile, open='at')
			write.table(boutagg, file=filex,  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
			close(filex)
		}

		closeAllConnections() 

	}
	}

}


mergeAdjacent <- function (lightBouts) {

	# only possible to merge if at least 2 bouts
	if (nrow(lightBouts)>1) {


	lightBouts$nextBoutStart = lightBouts$startdate
	lightBouts$nextBoutStart[1:(nrow(lightBouts)-1)] = lightBouts$startdate[2:nrow(lightBouts)]
	lightBouts$adjacent = as.numeric(difftime(lightBouts$nextBoutStart, lightBouts$enddate,  units="mins")) == 1

 
	ix = which(lightBouts$adjacent == TRUE)
	ixF = which(lightBouts$adjacent == FALSE)

	if (length(ix)>=1) {
	while (length(ix) >=1) {

		currentStartIdx = min(ix)
		currentStart = lightBouts[currentStartIdx,]

		currentEndIdx = min(ixF[which(ixF>currentStartIdx)])
		if (is.na(currentEndIdx)) {
			currentEndIdx = nrow(lightBouts)
		}

		currentEnd = lightBouts[currentEndIdx,]

		# join the bouts so the duration is the total across all the adjacent bouts
		# update in bout list
		lightBouts$enddate[currentStartIdx] = currentEnd$enddate
		lightBouts$duration_secs[currentStartIdx] = sum(lightBouts$duration_secs[currentStartIdx:currentEndIdx])		
		lightBouts$duration = lightBouts$duration_secs / 60


		# remove rows except the first one that is the combined bout
		lightBouts = lightBouts[-((currentStartIdx+1):currentEndIdx),]

		# update adjacent flag after this merge
		lightBouts$nextBoutStart = lightBouts$startdate
		lightBouts$nextBoutStart[1:(nrow(lightBouts)-1)] = lightBouts$startdate[2:nrow(lightBouts)]

	        lightBouts$adjacent = as.numeric(difftime(lightBouts$nextBoutStart, lightBouts$enddate,  units="mins")) == 1

		ix = which(lightBouts$adjacent == TRUE)
	        ixF = which(lightBouts$adjacent == FALSE)

	}
	}
	}
	return(lightBouts)
}



