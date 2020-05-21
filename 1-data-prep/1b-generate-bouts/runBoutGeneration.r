source('processing-functions.r')
source('getCompleteDays.r')
source('makeBouts.r')
source('makeBoutsForClass.r')
source('checkBout.r')
source('otherDayImputation.r')
source('setValidImputedDays.r')

##
## set args

args <- commandArgs(trailingOnly = TRUE)
idListFile=args[1]
epochCodeDir=args[2]
tmpDir = args[3]
accelDataDir = args[4]
boutsDir = args[5]
epochLength = as.integer(args[6])
continue=as.logical(args[7])

## set random seed
set.seed(1234)


## for year function for dates
library(data.table)

##
## list of participant id's for processing

ids = read.table(idListFile, sep=',', header=0)

print(continue)

# clear all derived files
if (is.null(continue) || continue == FALSE) {
	print('reinitiating derived files')
	clearAllDerivedFiles(boutsDir)
} else {
	derivedIds = read.table(paste(boutsDir, 'activity-summary.txt', sep=''), header=1, sep=',')
	# TODO update this when we fix row.names when adding row to activity summary
	derivedIds = derivedIds$id
}



for (id in ids$V1) {

	if (!is.null(continue) && continue == TRUE) {
		# check derived data not already generated

		if (length(which(derivedIds == id)) > 0) {
			next
		}
	}

	print(id)
        filename=paste(id, '_90001_0_0.cwa', sep='')

	result = tryCatch({

		##
		## get time series from CWA

		print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
		print(paste0('pre copy from rdsf: ', Sys.time()))

		## copy across data
		copyDataFromRDSF(accelDataDir, tmpDir, filename, boutsDir)

		print(paste0('post copy from rdsf: ', Sys.time()))

		## process CWA
		processCWA(tmpDir, filename, epochLength, boutsDir)

		print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
                print(paste0('post process CWA: ', Sys.time()))

		## extract from gz file
		extractTimeSeries(tmpDir, id, boutsDir)

		print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
		print(paste0('pre read: ', Sys.time()))

		## read in time series
		timeSeries = readTimeSeries(tmpDir, id, boutsDir)

		# fix for occasional rows where not imputed but no activity class
		ix = which(timeSeries$imputed == 0 & is.na(timeSeries$moderate))
		timeSeries$imputed[ix] = 1

		print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
		print(paste0('post read: ', Sys.time()))

	}, error = function(e) {
		print(e)
		simpleError('')
        })

#	# couldn't get time series so skip
#	if(inherits(result, "error")){
#		next
#	}

	if (is.null(timeSeries)) {
		write.table(id, paste(boutsDir, 'errors/errors-no-timeseries.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)
		next
	}


	result = tryCatch({

		##
		## generate bouts on imputed data

		# for imputed analysis 72 hours of non-imputed wear time is needed
		print('Imputed version')

		timeSeriesImp = otherDayImputation(timeSeries)
		timeSeriesImp = setValidImputedDays(timeSeriesImp, epochLength)

		print(unique(na.omit(timeSeriesImp$dayidx)))
		numCompleteDaysImputed = length(unique(na.omit(timeSeriesImp$dayidx)))
		print(paste('num complete days imputed:', numCompleteDaysImputed))

		if (numCompleteDaysImputed>=2) {
#		if (numCompleteDaysImputed==2) {
			bouts100Imp = makeBouts(timeSeriesImp, epochLength)
			outfile=paste(boutsDir, 'bouts100-imp-', id, '.csv', sep='')
			write.table(bouts100Imp, outfile, sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE)

			boutsClassImp = makeBoutsForClass(timeSeriesImp, epochLength)
			outfile=paste(boutsDir, 'boutsClass-imp-', id, '.csv', sep='')
		        write.table(boutsClassImp, outfile, sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE)
			write.table(id, paste(boutsDir, 'imp-ids-inc.txt', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)

			# overall time spent in activity types
			

		}
		else {
			write.table(id, paste(boutsDir, 'imp-ids-exc.txt', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
		}
		print('imputed bout generation end')

		##
		## generate bouts on complete days data

		## make 'complete days' dataset version
		timeSeriesCD = getCompleteDays(timeSeries, epochLength)

		
		print(unique(na.omit(timeSeriesCD$dayidx)))

		# for complete days analysis 3 complete days of non-imputed wear time is needed
		numCompleteDays = length(unique(na.omit(timeSeriesCD$dayidx)))
		print(paste('num complete days:', numCompleteDays))

		if (numCompleteDays>=2) {
			print('Complete days version')

			# MVPA bouts using 100mg threshold
			bouts100CD = makeBouts(timeSeriesCD, epochLength)
			outfile=paste(boutsDir, 'bouts100-CD-', id, '.csv', sep='')
		        write.table(bouts100CD, outfile, sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE)

			# predicted classes from AD model
			boutsClassCD = makeBoutsForClass(timeSeriesCD, epochLength)
			outfile=paste(boutsDir, 'boutsClass-CD-', id, '.csv', sep='')
		        write.table(boutsClassCD, outfile, sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE)
			write.table(id, paste(boutsDir, 'cd-ids-inc.txt', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
		}
		else {
			write.table(id, paste(boutsDir, 'cd-ids-exc.txt', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
		}

		print('complete days bout generation end')

		## calculate the AVM for the complete days and imputed versions, and AD approach (using all imputed data)

		# complete days		
		ixValidCD = which(timeSeriesCD$validDay == TRUE)
		completeDaysAVM = mean(timeSeriesCD$avm[ixValidCD])
		numEpochsValidCD = length(ixValidCD)
#		print(paste0('num CD valid epochs: ', numEpochsValidCD))
		competeDays100 = length(which(timeSeriesCD$avm[ixValidCD] >= 100))
		completeDaysNot100 = length(which(timeSeriesCD$avm[ixValidCD] >= 0 & timeSeriesCD$avm[ixValidCD] < 100))

		completeDaysClassMod = length(which(timeSeriesCD$moderate[ixValidCD] == 1)) 
		completeDaysClassSed = length(which(timeSeriesCD$sedentary[ixValidCD] == 1))		
		completeDaysClassSleep = length(which(timeSeriesCD$sleep[ixValidCD] == 1))
		completeDaysClassLight = length(which(timeSeriesCD$tasks.light[ixValidCD] == 1))
		completeDaysClassWalk = length(which(timeSeriesCD$walking[ixValidCD] == 1))

		# imputed
		ixValidImp = which(timeSeriesImp$validDay == TRUE)
		imputedAVM = mean(timeSeriesImp$avm[ixValidImp])
		numEpochsValidImp = length(ixValidImp)
#                print(paste0('num imp valid epochs: ', numEpochsValidImp))
                imputed100 = length(which(timeSeriesImp$avm[ixValidImp] >= 100)) 
                imputedNot100 = length(which(timeSeriesImp$avm[ixValidImp] >= 0 & timeSeriesImp$avm[ixValidImp] < 100))
                imputedClassMod = length(which(timeSeriesImp$moderate[ixValidImp] == 1))
                imputedClassSed = length(which(timeSeriesImp$sedentary[ixValidImp] == 1))
                imputedClassSleep = length(which(timeSeriesImp$sleep[ixValidImp] == 1))
                imputedClassLight = length(which(timeSeriesImp$tasks.light[ixValidImp] == 1))
                imputedClassWalk = length(which(timeSeriesImp$walking[ixValidImp] == 1))

		# ad avm
		adAVM = mean(timeSeries$avm, na.rm=TRUE)
	
		summaryRow = paste(id, numCompleteDaysImputed>=2, numCompleteDaysImputed, numCompleteDays>=2, numCompleteDays, adAVM, completeDaysAVM, numEpochsValidCD, competeDays100, completeDaysNot100, completeDaysClassMod, completeDaysClassSed, completeDaysClassSleep, completeDaysClassLight, completeDaysClassWalk, imputedAVM, numEpochsValidImp, imputed100, imputedNot100, imputedClassMod, imputedClassSed, imputedClassSleep, imputedClassLight, imputedClassWalk, sep=',')

		write.table(summaryRow, paste(boutsDir, 'activity-summary.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE, row.names=FALSE)


	}, error = function(e) {
                print("ERROR could make bouts")
		print(e)
                write.table(id, paste(boutsDir, 'errors/errors-bouts.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)
#                next
        })

	print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
	print(paste0('post processing: ', Sys.time()))

	## clean up tmp directory

	cleanUpFiles(tmpDir, id)

	print(paste0('post clean: ', Sys.time()))

}



warnings()
