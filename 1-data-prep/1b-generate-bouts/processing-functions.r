
copyDataFromRDSF <- function(accelDataDir, tmpDir, filename, boutsDir) {

fileCopyCMD = paste('scp newblue1://', accelDataDir, '/', filename, ' ', tmpDir, filename, sep='')

        result = tryCatch({
               system(fileCopyCMD)
        }, error = function(e) {
                print("ERROR could not scp file into BlueCrystal scratch space")
                print(fileCopyCMD)
                write.table(id, paste(boutsDir, 'errors/errors-copy.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)

		simpleError('')
        })
}



processCWA <- function(tmpDir, filename, epochLength, boutsDir) {

	setwd(epochCodeDir)
        pythonCMD=paste('python accProcess.py ', tmpDir, filename, ' --epochPeriod ', epochLength, ' --timeSeriesDateColumn True --deleteIntermediateFiles False',sep='')

	print(pythonCMD)

        result = tryCatch({
               system(pythonCMD)
        }, error = function(e) {
                print("ERROR could not process CWA file")
                print(pythonCMD)
		write.table(id, paste(boutsDir, 'errors/errors-process.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)
		
		simpleError('')
        })

}

extractTimeSeries <- function(tmpDir, id, boutsDir) {

	extractCMD=paste('gunzip ', tmpDir, id, '_90001_0_0-timeSeries.csv.gz', sep='')
        result = tryCatch({
               system(extractCMD)
        }, error = function(e) {
                print("ERROR could not gunzip time series file")
                print(extractCMD)
                write.table(id, paste(boutsDir, 'errors/errors-extract.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)

		simpleError('')
        })

}

cleanUpFiles <- function(tmpDir, id, boutsDir) {
	cleanCMD=paste('rm ', tmpDir, id, '_90001_0_0*', sep='')
	result = tryCatch({
               system(cleanCMD)
        }, error = function(e) {
                print("ERROR could not clean up files")
                print(cleanCMD)
                write.table(id, paste(boutsDir, 'errors/errors-cleanup.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)

		simpleError('')
        })

}

readTimeSeries <- function(tmpDir, id, boutsDir) {
	result = tryCatch({
                timeSeriesFile = paste(tmpDir, id, '_90001_0_0-timeSeries.csv', sep='')
                timeSeries = read.table(timeSeriesFile, sep=',', header=1)
                colnames(timeSeries)[which(substring(colnames(timeSeries), 1, 12)=="acceleration")] = 'avm'
                timeSeries$time = strptime(timeSeries$time, format='%Y-%m-%d %H:%M:%OS')

                # for some reason AVM is 1000 times higher than milli-gravities - WHY IS THIS?
                timeSeries$avm = timeSeries$avm/1000
                print(paste('avg AVM: ', mean(timeSeries$avm, na.rm=TRUE), sep=''))
                print(paste('min AVM: ', min(timeSeries$avm, na.rm=TRUE), sep=''))
                print(paste('max AVM: ', max(timeSeries$avm, na.rm=TRUE), sep=''))

		return(timeSeries)
        }, error = function(e) {
                print("ERROR could not read time series file")
                write.table(id, paste(boutsDir, 'errors/errors-read.txt', sep=''),  sep=',', col.names=FALSE, quote=FALSE, append=TRUE)

		simpleError('')
        })

}


clearAllDerivedFiles <- function(boutsDir) {

	summaryRow = "id,imputed_inc,num_imputed_days,completedays_inc,num_complete_days,adAVM,completeDaysAVM,numEpochsValidCD,completeDays100,completeDaysNot100,completeDays3mets,completeDaysClassMod,completeDaysClassSed,completeDaysClassSleep,completeDaysClassLight,completeDaysClassWalk,imputedAVM,numEpochsValidImp,imputed100,imputedNot100,imputed3mets,imputedClassMod,imputedClassSed,imputedClassSleep,imputedClassLight,imputedClassWalk"
	write.table(summaryRow, paste(boutsDir, 'activity-summary.txt', sep=''),  sep=',', col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE)
	cat(NULL,file=paste(boutsDir, 'cd-ids-inc.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'cd-ids-exc.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'imp-ids-inc.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'imp-ids-exc.txt', sep=''))


	cat(NULL,file=paste(boutsDir, 'errors/errors-read.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'errors/errors-copy.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'errors/errors-extract.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'errors/errors-process.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'errors/errors-cleanup.txt', sep=''))
	cat(NULL,file=paste(boutsDir, 'errors/errors-no-timeseries.txt', sep=''))

}
