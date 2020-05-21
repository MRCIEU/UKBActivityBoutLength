

## version is either CD or imp
aggregateBouts <- function(version, i, daytimeonly=FALSE) {


derivedDataDir=paste(Sys.getenv('HOME'), '/2016-biobank-accelerometer/data/accel/derived/', sep='')
boutDir=paste(derivedDataDir, 'activityBouts/', sep='')
aggDir=paste(derivedDataDir, 'boutAggregationsWithN-predicted-20190829/', sep='')

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
	print(paste(subdir, 'activity-summary.txt', sep=''))

	##### is this needed?
	activitySummary$num_imputed_days = as.integer(activitySummary$num_imputed_days)
	activitySummary$num_complete_days = as.integer(activitySummary$num_complete_days)


	## load each persons bouts and aggregate them
	for (ix in 1: nrow(idlistinc)) {

		id = idlistinc$V1[ix]

		###
                ### moderate class prediction

		# get bouts for this person
		bouts = read.table(paste(subdir, 'boutsClass-',version,'-', id, '.csv', sep=''), header=0, sep=',', col.names=c('boutidx', 'duration_secs', 'auc', 'complexity', 'class', 'valid', 'partial', 'startdate', 'enddate', 'boutContainsImputeBound', 'edgeImputeBound'))
                bouts$duration = bouts$duration_secs / 60
                bouts$auc = bouts$auc * 60

                # aggregate bouts walking
		print('walk')
		boutsWalk = bouts[which(bouts$class == 'walking'),]
                boutaggWalk = aggregateBoutsForPerson(boutsWalk, daytimeonly)
                colnames(boutaggWalk) <- paste(colnames(boutaggWalk), '_classWalk', sep = "_")

		# aggregate bouts moderate
		print('mod')
		boutsMod = bouts[which(bouts$class == 'moderate'),]
                boutaggMod = aggregateBoutsForPerson(boutsMod, daytimeonly)
                colnames(boutaggMod) <- paste(colnames(boutaggMod), '_classMod', sep = "_")

		# aggregate bouts sedentary
		print('sed')
		boutsSed = bouts[which(bouts$class == 'sedentary'),]
                boutaggSed = aggregateBoutsForPerson(boutsSed, daytimeonly)
                colnames(boutaggSed) <- paste(colnames(boutaggSed), '_classSed', sep = "_")

		# aggregate bouts tasks.light
		print('light')
		boutsLight = bouts[which(bouts$class == 'tasks.light'),]
                boutaggLight = aggregateBoutsForPerson(boutsLight, daytimeonly)
                colnames(boutaggLight) <- paste(colnames(boutaggLight), '_classLight', sep = "_")

		# aggregate bouts sleep
		print('sleep')
		boutsSleep = bouts[which(bouts$class == 'sleep'),]
                boutaggSleep = aggregateBoutsForPerson(boutsSleep, daytimeonly)
                colnames(boutaggSleep) <- paste(colnames(boutaggSleep), '_classSleep', sep = "_")


		# combine data together
		boutagg = cbind.data.frame(id, boutaggSleep, boutaggWalk, boutaggMod, boutaggSed, boutaggLight, activitySummary[which(activitySummary$id == id),])


		if (ix==1) {
			print('first')
			filex = file(aggFile, open='wt')
			write.table(boutagg, file=filex,  sep=',', col.names=TRUE, row.names=FALSE, quote=FALSE, append=FALSE)
			close(filex)
		}
		else {
			filex = file(aggFile, open='at')
			write.table(boutagg, file=filex,  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
			close(filex)
		}

	}
	}

}
