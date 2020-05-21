


getCompleteDays <- function(timeSeries, epochLength) {

	# get unique dates (without times)
	timeSeries$date = paste(timeSeries$time$mday, "/", timeSeries$time$mon+1, "/" , year(timeSeries$time), sep='')
	uniqueDates = unique(timeSeries$date)

	# for each date exclude if it contains imputed data


	timeSeries$validDay=FALSE
	currentstartix=1
	dayidx=1
	timeSeries$dayidx = NA

	while(!is.na(currentstartix)) {

		currentstart = timeSeries$time[currentstartix]

		# add 1 day to date, and minus 1 epoch
		startnext = incrementDay(currentstart)

		d = timeSeries$date[currentstartix]
		
		ixstart = which(timeSeries$time == currentstart)
		ixstartnext = which(timeSeries$time == startnext)
		if (length(ixstartnext)==0) {
                        ixstartnext = NA
                }

		ixend = which(timeSeries$time == (startnext - epochLength))

		if (length(ixend)==0) {

			## hack for last day that is 1 epoch short
			ixend = which(timeSeries$time == (startnext - epochLength*2))
			if (length(ixend)==0) {
				break
			}
		}

		timeSeriesForDate = timeSeries[ixstart:ixend,]
		
		## check it has the correct number of values for this day

		numEpochsInDay = nrow(timeSeriesForDate)
		correctNumEpochs = (24*60*60)/epochLength
                stopifnot(((numEpochsInDay == correctNumEpochs) || (numEpochsInDay == (correctNumEpochs-1))))


		## look for imputed regions
		ix = which(timeSeriesForDate$imputed==1)

		if (length(ix)==0) {
			timeSeries$validDay[ixstart:ixend] = TRUE
			timeSeries$dayidx[ixstart:ixend] = dayidx
			dayidx=dayidx+1
		}

		currentstartix = ixstartnext
	}

	return(timeSeries)

}


incrementDay <- function(time) {

	timeNew = as.POSIXlt(time + 60*60*24)

	## hour changes if the clocks change
	if (timeNew$hour!=time$hour) {
		# mon+1 because retrieved zero based but set 1-based
		t2str = paste(timeNew$mday, "/", timeNew$mon+1, "/" , year(timeNew), " ", time$hour, ":", timeNew$min, sep="")
		timeNew = strptime(t2str, format='%d/%m/%Y %H:%M')
	}

	return(timeNew)
}

