source('../processing-functions.r')
source('../getCompleteDays.r')

##
## set args

args <- commandArgs(trailingOnly = TRUE)
idListFile=args[1]
epochCodeDir=args[2]
tmpDir = args[3]
accelDataDir = args[4]
boutsDir = args[5]
epochLength = as.integer(args[6])

## set random seed
set.seed(1234)


## for year function for dates
library(data.table)

##
## list of participant id's for processing

ids = read.table(idListFile, sep=',', header=0)


# write header in venn data file

vennRow = "id, bothMVPA, bothNotMVPA, predictedMVPAOnly, thresholdMVPAOnly"
write.table(vennRow, paste(boutsDir, 'vennData.csv', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE)


for (id in ids$V1) {
	print(id)
        filename=paste(id, '_90001_0_0.cwa', sep='')

	result = tryCatch({

		##
		## get time series from CWA

		## copy across data
		copyDataFromRDSF(accelDataDir, tmpDir, filename, boutsDir)

		## process CWA
		processCWA(tmpDir, filename, epochLength, boutsDir)

		## extract from gz file
		extractTimeSeries(tmpDir, id, boutsDir)

		## read in time series
		timeSeries = readTimeSeries(tmpDir, id, boutsDir)

		# fix for occasional rows where not imputed but no activity class
		ix = which(timeSeries$imputed == 0 & is.na(timeSeries$moderate))
		timeSeries$imputed[ix] = 1

	}, error = function(e) {
		simpleError('')
        })

	# couldn't get time series so skip
	if(inherits(result, "error")){
		next
	}


		##
		## generate bouts on complete days data

		## make 'complete days' dataset version
		timeSeriesCD = getCompleteDays(timeSeries, epochLength)

		numdays = length(unique(na.omit(timeSeriesCD$dayidx)))
		if (numdays == 0) {
			# SHOULD NOT GET HERE
			print('no valid days. skipping ...')
			next
		}

		# for complete days analysis 3 complete days of non-imputed wear time is needed
		numCompleteDays = max(timeSeriesCD$dayidx, na.rm=TRUE)

		print(unique(timeSeriesCD$dayidx))
		print(paste('num complete days:', numCompleteDays))



		## select 2 complete days randomly
		daysInc = unique(na.omit(timeSeriesCD$dayidx))
		print('valid days')
		print(daysInc)

		randDayIdx = sample(1:length(daysInc), 2, replace=FALSE)
		print('indexes chosen')
		print(randDayIdx)

		randDays = daysInc[randDayIdx]
		print('days chosen')
		print(randDays)

		ix = which(timeSeriesCD$dayidx %in% randDays)
		print('time points in chosen days:')
		print(length(ix))

		timeSeriesCD = timeSeriesCD[ix,]
		
		bothMVPA = length(which(timeSeriesCD$moderate == 1 & timeSeriesCD$avm>=100))
		bothNotMVPA = length(which(timeSeriesCD$moderate == 0 & timeSeriesCD$avm<100))
		predictedMVPAOnly = length(which(timeSeriesCD$moderate == 1 & timeSeriesCD$avm<100))
		thresholdMVPAOnly = length(which(timeSeriesCD$moderate == 0 & timeSeriesCD$avm>=100))

		vennRow = paste(id, bothMVPA, bothNotMVPA, predictedMVPAOnly, thresholdMVPAOnly, sep=',')
		write.table(vennRow, paste(boutsDir, 'vennData.csv', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)

}



warnings()
